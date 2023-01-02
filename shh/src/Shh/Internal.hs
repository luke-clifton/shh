{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

-- | See documentation for "Shh".
module Shh.Internal where

import Prelude hiding (lines, unlines)

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.DeepSeq (force,NFData)
import Control.Exception as C
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import Data.ByteString.Lazy (ByteString, hGetContents, toStrict, fromStrict)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BC8
import qualified Data.ByteString.Lazy.Search as Search
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Char (isLower, isSpace, isAlphaNum, ord)
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Typeable
import GHC.IO.BufferedIO
import GHC.IO.Device as IODevice hiding (read)
import GHC.IO.Encoding
import GHC.Foreign (peekCStringLen, newCStringLen)
import GHC.IO.Exception (IOErrorType(ResourceVanished))
import GHC.IO.Handle hiding (hGetContents)
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import GHC.Stack
import Language.Haskell.TH
import qualified System.Directory as Dir
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, (</>))
import System.IO (IOMode(..), withFile, withBinaryFile, stderr, stdout, stdin)
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Error
import System.Posix.Signals
import System.Process
import Text.Printf

-- $setup
-- For doc-tests. Not sure I can use TH in doc tests.
-- >>> :seti -XOverloadedStrings
-- >>> import Data.Monoid
-- >>> import Data.ByteString.Lazy.Char8 (lines)
-- >>> let cat = exe "cat"
-- >>> let echo = exe "echo"
-- >>> let false = exe "false"
-- >>> let head = exe "head"
-- >>> let md5sum = exe "md5sum"
-- >>> let printf = exe "printf"
-- >>> let sleep = exe "sleep"
-- >>> let true = exe "true"
-- >>> let wc = exe "wc"
-- >>> let xargs = exe "xargs"
-- >>> let yes = exe "yes"
-- >>> let some_command = writeOutput "this is stdout" >> (writeOutput "this is stderr" &> StdErr)

-- | This function needs to be called in order to use the library successfully
-- from GHCi. If you use the @formatPrompt@ function from the @shh-extras@
-- package, this will be automatically called for you.
initInteractive :: IO ()
initInteractive = do
    hSetBuffering stdin LineBuffering

-- | When a process exits with a non-zero exit code
-- we throw this @Failure@ exception.
--
-- The only exception to this is when a process is terminated
-- by @SIGPIPE@ in a pipeline, in which case we ignore it.
data Failure = Failure
    { failureProg   :: ByteString
    , failureArgs   :: [ByteString]
    , failureStack  :: CallStack
    , failureCode   :: Int
    -- | Failure can optionally contain the stderr of a process.
    , failureStdErr :: Maybe ByteString
    }

instance Show Failure where
    show f = concat $
        [ "Command `"
        ]
        ++ [unwords (toString (failureProg f) : map show (failureArgs f))]
        ++
        [ "` failed [exit "
        , show (failureCode f)
        , "] at "
        , prettyCallStack (failureStack f)
        ]
        ++ flip (maybe []) (failureStdErr f) (\s ->
           ["\n-- stderr --\n" ++ toString s])

instance Exception Failure

-- | This class is used to allow most of the operators in Shh to be
-- polymorphic in their return value. This makes using them in an `IO` context
-- easier (we can avoid having to prepend everything with a `runProc`).
class Shell f where
    runProc :: HasCallStack => Proc a -> f a

-- | Helper function that creates and potentially executes a @`Proc`@
buildProc :: Shell f => (Handle -> Handle -> Handle -> IO a) -> f a
buildProc = runProc . Proc

-- | Like @`|>`@ except that it keeps both return results. Be aware
-- that the @fst@ element of this tuple may be hiding a @SIGPIPE@
-- exception that will explode on you once you look at it.
--
-- You probably want to use @`|>`@ unless you know you don't.
pipe :: Shell f => Proc a -> Proc b -> f (a, b)
pipe (Proc a) (Proc b) = buildProc $ \i o e ->
    withPipe $ \r w -> do
        let
            a' = a i w e `finally` hClose w
            b' = b r o e `finally` hClose r
        concurrently a' b'

-- | Like @`pipe`@, but plumbs stderr. See the warning in @`pipe`@.
pipeErr :: Shell f => Proc a -> Proc b -> f (a, b)
pipeErr (Proc a) (Proc b) = buildProc $ \i o e -> do
    withPipe $ \r w -> do
        let
            a' = a i o w `finally` hClose w
            b' = b r o e `finally` hClose r
        concurrently a' b'


-- | Use this to send the output of one process into the input of another.
-- This is just like a shell's `|` operator.
--
-- The result is polymorphic in its output, and can result in either
-- another `Proc a` or an `IO a` depending on the context in which it is
-- used.
--
-- If any intermediate process throws an exception, the whole pipeline
-- is canceled.
--
-- The result of the last process in the chain is the result returned
-- by the pipeline. 
--
-- >>> echo "Hello" |> wc
--       1       1       6
(|>) :: Shell f => Proc a -> Proc b -> f b
a |> b = runProc $ fmap snd (a `pipe` b)
infixl 1 |>


-- | Similar to `|>` except that it connects stderr to stdin of the
-- next process in the chain.
--
-- NB: The next command to be `|>` on will recapture the stdout of
-- both preceding processes, because they are both going to the same
-- handle!
--                                            
-- See the `&>` and `&!>` operators for redirection.
--
-- >>> echo "Ignored" |!> wc "-c"
-- Ignored
-- 0
(|!>) :: Shell f => Proc a -> Proc b -> f b
a |!> b = runProc $ fmap snd (a `pipeErr` b)
infixl 1 |!>

-- | Things that can be converted to a @`FilePath`@.
--
-- The results must use the file system encoding. Use this
-- if you want to pass a @ByteString@ to @`System.IO.openFile`@,
-- or if you want to turn a @FilePath@ into a @ByteString@.
--
-- If you never change the file system encoding, it should be safe to use
-- @`unsafePerformIO`@ on these functions.
class ToFilePath a where
    toFilePath :: a -> IO FilePath
    fromFilePath :: FilePath -> IO a

instance ToFilePath FilePath where
    toFilePath = pure
    fromFilePath = pure

instance ToFilePath ByteString.ByteString where
    toFilePath bs = do
        enc <- getFileSystemEncoding
        ByteString.useAsCStringLen bs (peekCStringLen enc)
    fromFilePath fp = do
        enc <- getFileSystemEncoding
        newCStringLen enc fp >>= ByteString.unsafePackMallocCStringLen

instance ToFilePath ByteString where
    toFilePath = toFilePath . toStrict
    fromFilePath = fmap fromStrict . fromFilePath

--
-- | Redirect stdout of this process to another location
--
-- >>> echo "Ignore me" &> Append "/dev/null"
(&>) :: Shell f => Proc a -> Stream -> f a
p &> StdOut = runProc p
(Proc f) &> StdErr = buildProc $ \i _ e -> f i e e
(Proc f) &> (Truncate path) = buildProc $ \i _ e -> do
    path' <- toFilePath path
    withBinaryFile path' WriteMode $ \h -> f i h e
(Proc f) &> (Append path) = buildProc $ \i _ e -> do
    path' <- toFilePath path
    withBinaryFile path' AppendMode $ \h -> f i h e
infixl 9 &>

-- | Redirect stderr of this process to another location
--
-- >>> echo "Shh" &!> StdOut
-- Shh
(&!>) :: Shell f => Proc a -> Stream -> f a
p &!> StdErr = runProc p
(Proc f) &!> StdOut = buildProc $ \i o _ -> f i o o
(Proc f) &!> (Truncate path) = buildProc $ \i o _ -> do
    path' <- toFilePath path
    withBinaryFile path' WriteMode $ \h -> f i o h
(Proc f) &!> (Append path) = buildProc $ \i o _ -> do
    path' <- toFilePath path
    withBinaryFile path' AppendMode $ \h -> f i o h
infixl 9 &!>

-- | Lift a Haskell function into a @`Proc`@. The handles are the @stdin@
-- @stdout@ and @stderr@ of the resulting @`Proc`@
nativeProc :: (Shell f, NFData a) => (Handle -> Handle -> Handle -> IO a) -> f a
nativeProc f = runProc $ Proc $ \i o e -> handle handler $ do
    -- We duplicate these so that you can't accidentally close the
    -- real ones.
    withDuplicates i o e $ \i' o' e' -> do
        (f i' o' e' >>= C.evaluate . force)
            `finally` hClose i'
            `finally` hClose o'
            `finally` hClose e'

    where
        -- The resource vanished error only occurs when upstream pipe closes.
        -- This can only happen with the `|>` combinator, which will discard
        -- the result of this `Proc` anyway. If the return value is somehow
        -- inspected, or maybe if the exception is somehow legitimate, we
        -- simply package it up as an exploding return value. `runProc` will
        -- make sure to evaluate all `Proc`'s to WHNF in order to uncover it.
        -- This should never happen. *nervous*
        handler :: IOError -> IO a
        handler e
            | ioeGetErrorType e == ResourceVanished = pure (throw e)
            | otherwise = throwIO e

-- | Flipped version of `|>` with lower precedence.
--
-- >>> captureTrim <| (echo "Hello" |> wc "-c")
-- "6"
(<|) :: Shell f => Proc a -> Proc b -> f a
(<|) = flip (|>)
infixr 1 <|

-- | Create a pipe, and close both ends on exception. The first argument
-- is the read end, the second is the write end.
--
-- >>> withPipe $ \r w -> hPutStr w "test" >> hClose w >> hGetLine r
-- "test"
withPipe :: (Handle -> Handle -> IO a) -> IO a
withPipe k =
    bracket
        createPipe
        (\(r,w) -> hClose r `finally` hClose w)
        (\(r,w) -> k r w)

-- | Simple @`Proc`@ that writes its argument to its @stdout@. This behaves
-- very much like the standard @printf@ utility, except that there is no
-- restriction as to what can be in the argument.
--
-- NB: @String@ arguments are encoded as UTF8, while @ByteString@ is passed
-- through. Be aware if you are using @OverloadedStrings@ that you will get
-- wrong results if using unicode in your string literal and it inferes
-- anything other than @String@.
--
-- >>> writeOutput "Hello"
-- Hello
writeOutput :: (ExecArg a, Shell io) => a -> io ()
writeOutput s = nativeProc $ \_ o _ -> do
    mapM_ (BS.hPutStr o) (asArg s)

-- | Simple @`Proc`@ that writes its argument to its @stderr@.
-- See also @`writeOutput`@.
--
-- >>> writeError "Hello" &> devNull
-- Hello
writeError :: (ExecArg a, Shell io) => a -> io ()
writeError s = nativeProc $ \_ _ e -> do
   mapM_ (BS.hPutStr e) (asArg s)

-- | Simple @`Proc`@ that reads its input, and can react to it with an IO
-- action. Does not write anything to its output. See also @`capture`@.
--
-- @`readInput`@ uses lazy IO to read its stdin, and works with infinite
-- inputs.
--
-- >>> yes |> readInput (pure . unlines . take 3 . lines)
-- "y\ny\ny\n"
readInput :: (NFData a, Shell io) => (ByteString -> IO a) -> io a
readInput f = nativeProc $ \i _ _ -> do
    hGetContents i >>= f

-- | Join a list of @ByteString@s with newline characters, terminating it
-- with a newline.
unlines :: [ByteString] -> ByteString
unlines = toLazyByteString . mconcat . map (\l -> lazyByteString l <> char7 '\n')

-- | Like @`readInput`@, but @`endBy`@s the string.
--
-- >>> yes |> readInputEndBy "\n" (pure . take 3)
-- ["y","y","y"]
readInputEndBy :: (NFData a, Shell io) => ByteString -> ([ByteString] -> IO a) -> io a
readInputEndBy s f = readInput (f . endBy s)

-- | Like @`readInput`@, but @`endBy`@s the string on the 0 byte.
--
-- >>> writeOutput "1\0\&2\0" |> readInputEndBy0 pure
-- ["1","2"]
readInputEndBy0 :: (NFData a, Shell io) => ([ByteString] -> IO a) -> io a
readInputEndBy0 = readInputEndBy "\0"

-- | Like @`readInput`@, but @`endBy`@s the string on new lines.
--
-- >>> writeOutput "a\nb\n" |> readInputLines pure
-- ["a","b"]
readInputLines :: (NFData a, Shell io) => ([ByteString] -> IO a) -> io a
readInputLines = readInputEndBy "\n"

-- | Creates a pure @`Proc`@ that simple transforms the @stdin@ and writes
-- it to @stdout@. The input can be infinite.
--
-- >>> yes |> pureProc (BS.take 4) |> capture
-- "y\ny\n"
pureProc :: Shell io => (ByteString -> ByteString) -> io ()
pureProc f = nativeProc $ \i o _ -> do
    s <- hGetContents i
    BS.hPutStr o (f s)

-- | Captures the stdout of a process and prefixes all the lines with
-- the given string.
--
-- >>> some_command |> prefixLines "stdout: " |!> prefixLines "stderr: " &> StdErr
-- stdout: this is stdout
-- stderr: this is stderr
prefixLines :: Shell io => ByteString -> io ()
prefixLines s = pureProc $ \inp -> toLazyByteString $
    mconcat $ map (\l -> lazyByteString s <> lazyByteString l <> char7 '\n') (BC8.lines inp)

-- | Provide the stdin of a `Proc` from a `ByteString`
--
-- Same as @`writeOutput` s |> p@
writeProc :: Shell io => Proc a -> ByteString -> io a
writeProc p s = writeOutput s |> p

-- | Run a process and capture its output lazily. Once the continuation
-- is completed, the handles are closed. However, the process is run
-- until it naturally terminates in order to capture the correct exit
-- code. Most utilities behave correctly with this (e.g. @cat@ will
-- terminate if you close the handle).
--
-- Same as @p |> readInput f@
withRead :: (Shell f, NFData b) => Proc a -> (ByteString -> IO b) -> f b
withRead p f = p |> readInput f

-- | Type used to represent destinations for redirects. @`Truncate` file@
-- is like @> file@ in a shell, and @`Append` file@ is like @>> file@.
data Stream = StdOut | StdErr | Truncate ByteString | Append ByteString

-- | Shortcut for @`Truncate` "\/dev\/null"@
-- 
-- >>> echo "Hello" &> devNull
devNull :: Stream
devNull = Truncate "/dev/null"

-- | Type representing a series or pipeline (or both) of shell commands.
--
-- @Proc@'s can communicate to each other via @stdin@, @stdout@ and @stderr@
-- and can communicate to Haskell via their parameterised return type, or by
-- throwing an exception.
newtype Proc a = Proc (Handle -> Handle -> Handle -> IO a)
    deriving Functor

instance MonadIO Proc where
    liftIO a = buildProc $ \_ _ _ -> a

-- | The `Semigroup` instance for `Proc` pipes the stdout of one process
-- into the stdin of the next. However, consider using `|>` instead which
-- behaves when used in an `IO` context. If you use `<>` in an IO monad
-- you will be using the `IO` instance of semigroup which is a sequential
-- execution. `|>` prevents that error.
instance Semigroup (Proc a) where
    (<>) = (|>)

instance (a ~ ()) => Monoid (Proc a) where
    mempty = buildProc $ \_ _ _ -> pure ()

instance Applicative Proc where
    pure a = buildProc $ \_ _ _  -> do
        pure a

    f <*> a = do
        f' <- f
        f' <$> a
        
instance Monad Proc where
    (Proc a) >>= f = buildProc $ \i o e -> do
        ar <- a i o e
        let
            Proc f' = f ar
        f' i o e

instance Shell IO where
    runProc = runProc' stdin stdout stderr

instance Shell Proc where
    runProc = id

-- | Runs a `Proc` in `IO`. Like `runProc`, but you get to choose the handles.
-- This is UNSAFE to expose externally, because there are restrictions on what
-- the Handle can be. Within shh, we never call `runProc'` with invalid handles,
-- so we ignore that corner case (see `hDup`).
runProc' :: Handle -> Handle -> Handle -> Proc a -> IO a
runProc' i o e (Proc f) = do
    -- Flush stdout and stderr so that sequencing commands with
    -- Haskell IO functions looks right.
    hFlush stdout
    hFlush stderr
    r <- f i o e
    -- Evaluate to WHNF to uncover any ResourceVanished exceptions
    -- that may be hiding in there from `nativeProc`. These should
    -- not happen under normal circumstances, but we would at least
    -- like to have the exception thrown ASAP if, for whatever reason,
    -- it does happen.
    pure $! r

-- | Create a `Proc` from a command and a list of arguments.
-- The boolean represents whether we should delegate control-c
-- or not. Most uses of @`mkProc'`@ in Shh do not delegate control-c.
{-# DEPRECATED mkProc' "Use mkProcWith instead" #-}
mkProc' :: HasCallStack => Bool -> ByteString -> [ByteString] -> Proc ()
mkProc' delegate = mkProcWith defaultProcOptions { delegateCtlc = delegate }

-- | Options for making processes.
data ProcOptions = ProcOptions
    { delegateCtlc :: Bool -- ^ Delegate control-c handling to the child.
    , closeFds     :: Bool -- ^ Close file descriptors before @exec@ing.
    }

-- | Default ProcOptions as used by most of this library.
defaultProcOptions :: ProcOptions
defaultProcOptions = ProcOptions
    { delegateCtlc = True
    , closeFds     = True
    }

-- | Create a `Proc` with custom options.
mkProcWith :: HasCallStack => ProcOptions -> ByteString -> [ByteString] -> Proc ()
mkProcWith options cmd args = Proc $ \i o e -> do
    cmd' <- toFilePath cmd
    args' <- mapM toFilePath args
    bracket
        (createProcess_ cmd' (proc cmd' args')
            { std_in = UseHandle i
            , std_out = UseHandle o
            , std_err = UseHandle e
            , close_fds = closeFds options
            , delegate_ctlc = delegateCtlc options
            }
        )
        (\(_,_,_,ph) -> terminateProcess ph >> waitForProcess ph)
        $ \(_,_,_,ph) -> waitProc cmd args ph `onException` (terminateProcess ph >> waitForProcess ph)

-- | Create a `Proc` from a command and a list of arguments. Does not delegate
-- control-c handling.
mkProc :: HasCallStack => ByteString -> [ByteString] -> Proc ()
mkProc = mkProcWith defaultProcOptions

-- | A special `Proc` which captures its stdin and presents it as a `ByteString`
-- to Haskell.
--
-- >>> printf "Hello" |> md5sum |> capture
-- "8b1a9953c4611296a827abf8c47804d7  -\n"
--
-- This is just @`readInput` pure@. Note that it is not lazy, and will read
-- the entire @ByteString@ into memory.
capture :: Shell io => io ByteString
capture = readInput pure

-- | Like @'capture'@, except that it @'trim'@s leading and trailing white
-- space.
--
-- >>> printf "Hello" |> md5sum |> captureTrim
-- "8b1a9953c4611296a827abf8c47804d7  -"
captureTrim :: Shell io => io ByteString
captureTrim = readInput (pure . trim)

-- | Like @'capture'@, but splits the input using the provided separator.
--
-- NB: This is strict. If you want a streaming version, use `readInput`
captureEndBy :: Shell io => ByteString -> io [ByteString]
captureEndBy s = readInput (pure . endBy s)

-- | Same as @'captureEndBy' "\\0"@.
captureEndBy0 :: Shell io => io [ByteString]
captureEndBy0 = captureEndBy "\0"

-- | Same as @'captureSplit' "\\n"@.
captureLines :: Shell io => io [ByteString]
captureLines = captureEndBy "\n"

-- | Capture stdout, splitting it into words.
captureWords :: Shell io => io [ByteString]
captureWords = readInput (pure . BC8.words)

-- | Capture stdout, and attempt to @`read`@ it
captureRead :: (Shell io, Read a, NFData a) => io a
captureRead = readInput (pure . read . toString)

-- | Apply a `Proc` to a `ByteString`. That is, feed the bytestring to
-- the @stdin@ of the process and read the @stdout@.
--
-- >> apply md5sum "Hello"
-- "8b1a9953c4611296a827abf8c47804d7  -\n"
apply :: (ExecArg a, Shell io) => Proc v -> a -> io ByteString
apply p b = writeOutput b |> p |> capture

-- | Flipped, infix version of `writeProc`
(>>>) :: Shell io => ByteString -> Proc a -> io a
(>>>) = flip writeProc


-- | Infix version of `writeProc`
(<<<) :: Shell io => Proc a -> ByteString -> io a
(<<<) = writeProc

-- | Wait on a given `ProcessHandle`, and throw an exception of
-- type `Failure` if its exit code is non-zero (ignoring SIGPIPE)
waitProc :: HasCallStack => ByteString -> [ByteString] -> ProcessHandle -> IO ()
waitProc cmd arg ph = waitForProcess ph >>= \case
    ExitFailure c
        | fromIntegral c == negate sigPIPE -> pure ()
        | otherwise -> throwIO $ Failure cmd arg callStack c Nothing
    ExitSuccess -> pure ()


-- | Drop trailing characters from a @ByteString@ while the given predicate
-- matches.
--
-- >>> dropWhileEnd isSpace "a line \n"
-- "a line"
dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd p b = case BC8.unsnoc b of
    Just (i, l) -> if p l then dropWhileEnd p i else b
    Nothing     -> b

-- | Trim leading and tailing whitespace.
--
-- >>> trim " a string \n"
-- "a string"
trim :: ByteString -> ByteString
trim = dropWhileEnd isSpace . BC8.dropWhile isSpace

-- | Run a `Proc` action, catching any `Failure` exceptions
-- and returning them.
tryFailure :: Shell m => Proc a -> m (Either Failure a)
tryFailure (Proc f) = buildProc $ \i o e -> try $ f i o e

-- | Like @`tryFailure`@ except that it takes an exception predicate which
-- selects which exceptions to catch. Any exception not matching the predicate
-- (returning @Nothing@) is re-thrown.
tryFailureJust :: Shell m => (Failure -> Maybe b) -> Proc a -> m (Either b a)
tryFailureJust pr (Proc f) = buildProc $ \i o e -> tryJust pr (f i o e)

-- | Run a `Proc` with an action to take if an exception is thrown.
catchFailure :: Shell m => Proc a -> (Failure -> Proc a) -> m a
catchFailure (Proc f) pr = buildProc $ \i o e -> catch (f i o e) (runProc . pr)

-- | Like @`catchFailureJust`@ except that it takes an exception predicate
-- which selects which exceptions to catch. Any exceptions not matching the
-- predicate (returning @Nothing@) are re-thrown.
catchFailureJust :: Shell m => (Failure -> Maybe b) -> Proc a -> (b -> Proc a) -> m a
catchFailureJust pr (Proc f) h = buildProc $ \i o e -> catchJust pr (f i o e) (runProc . h)

-- | Apply a function that translates non-0 exit codes to results. Any code
-- that returns a @Nothing@ will be thrown as a @`Failure`@.
translateCode' :: Shell m => (Int -> Maybe b) -> Proc a -> m (Either b a)
translateCode' f = tryFailureJust (f . failureCode)

-- | Apply a function to non-0 exit codes to extract a result. If @Nothing@
-- is produced, the @`Failure`@ is thrown.
translateCode :: Shell m => (Int -> Maybe a) -> Proc a -> m a
translateCode f p = catchFailureJust (f . failureCode) p pure

-- | Capture the stderr of the proc, and attach it to any @`Failure`@
-- exceptions that are thrown. The stderr is also forwarded to downstream
-- processes, or the inherited stderr handle. Note that capturing stderr
-- inherently requires that the stderr is accumulated in memory, so be
-- careful about processes that dump a lot of information.
failWithStdErr :: Shell io => Proc a -> io a
failWithStdErr p = runProc $ do
    r <- tryFailure p `pipeErr` readInputP (\i -> do
        writeError i
        pure i
        )
    case r of
        (Right a, _) -> pure a
        (Left f, err) -> liftIO $ throwIO $ f {failureStdErr = Just err}

-- | Run a `Proc` action, ignoring any `Failure` exceptions.
-- This can be used to prevent a process from interrupting a whole pipeline.
--
-- >>> false |> (sleep "0.1" >> echo 1)
-- *** Exception: Command `false` failed [exit 1] at CallStack (from HasCallStack):
-- ...
--
-- >>> (ignoreFailure false) |> (sleep "0.1" >> echo 1)
-- 1
ignoreFailure :: (Functor m, Shell m) => Proc a -> m ()
ignoreFailure = void . tryFailure

-- | Run a `Proc` action returning the exit code of the process instead of
-- throwing an exception.
--
-- >>> exitCode false
-- 1
exitCode :: (Functor m, Shell m) => Proc a -> m Int
exitCode = fmap getCode . tryFailure
    where
        getCode (Right _) = 0
        getCode (Left  f) = failureCode f

-- | Run the @`Proc`@, but don't throw an exception if it exits with the
-- given code. Note, that from this point on, if the proc did fail with the
-- code, everything else now sees it as having exited with 0. If you need
-- to know the code, you have to use `exitCode`.
ignoreCode :: (Monad m, Shell m) => Int -> Proc a -> m ()
ignoreCode code p = catchFailureJust pr (void p) pure
    where
        pr f
            | failureCode f == code = Just ()
            | otherwise             = Nothing

-- | A class for things that can be converted to arguments on the command
-- line. The default implementation is to use `show` and then encode it using
-- the file system encoding.
class ExecArg a where
    asArg :: a -> [ByteString]
    default asArg :: Show a => a -> [ByteString]
    asArg a = [unsafePerformIO $ fromFilePath $ show a]

    -- God, I hate that String is [Char]...
    asArgFromList :: [a] -> [ByteString]
    default asArgFromList :: Show a => [a] -> [ByteString]
    asArgFromList = concatMap asArg

-- | The @Char@ and @String@ instances encode using the file system encoding.
instance ExecArg Char where
    asArg s = [unsafePerformIO $ fromFilePath [s]]
    asArgFromList s = [unsafePerformIO $ fromFilePath s]

-- | The @[Char]@/@String@ instance encodes using the file system encoding.
instance ExecArg a => ExecArg [a] where
    asArg = asArgFromList
    asArgFromList = concatMap asArg

instance ExecArg ByteString where
    asArg s = [s]

instance ExecArg ByteString.ByteString where
    asArg s = [BS.fromStrict s]

instance ExecArg Int
instance ExecArg Integer
instance ExecArg Word

-- | A class for building up a command.
class Command a where
    toArgs :: HasCallStack => [ByteString] -> a

instance (a ~ ()) => Command (Proc a) where
    toArgs (cmd:args) = mkProc cmd args
    toArgs _ = error "The impossible happened. How did you construct this?"

instance (ExecArg b, Command a) => Command (b -> a) where
    toArgs f i = toArgs $ f ++ asArg i

-- | Commands can be executed directly in IO
instance (a ~ ()) => Command (IO a) where
    toArgs = runProc . toArgs

instance Command [ByteString] where
    toArgs = id

instance Command [ByteString.ByteString] where
    toArgs = map toStrict

-- | This type represents a partially built command. Further arguments
-- can be supplied to it, or it can be turned into a `Proc` or directly
-- executed in a context which supports that (such as `IO`).
type Cmd = HasCallStack => forall a. (Command a) => a

-- | This function turns a `Cmd` into a list of @`ByteString`@s.
--
-- >>> displayCommand $ echo "Hello, world!"
-- ["echo","Hello, world!"]
displayCommand :: Cmd -> [ByteString]
displayCommand = \c -> toArgs c

-- | Get all executables on your `$PATH`.
pathBins :: IO [FilePath]
pathBins = map takeFileName <$> pathBinsAbs

-- | Get all uniquely named executables on your `$PATH` as absolute
-- file names. The uniqueness is determined by the filename, and not
-- the whole path. First one found wins.
pathBinsAbs :: IO [FilePath]
pathBinsAbs = do
    pathsVar <- Split.splitOn ":" <$> getEnv "PATH"
    paths <- filterM Dir.doesDirectoryExist pathsVar
    findBinsIn paths

-- | Get all uniquely named executables from the list of directories. Returns
-- a list of absolute file names.
findBinsIn :: [FilePath] -> IO [FilePath]
findBinsIn paths = do
    ps <- ordNubOn takeFileName . concat <$> mapM (\d -> fmap (\x -> d++('/':x)) <$> Dir.getDirectoryContents d) paths
    filterM (tryBool . fmap Dir.executable . Dir.getPermissions) ps

    where
        -- TODO: Eventually replace this with nubOrdOn (containers 0.6.0.1 dep)
        ordNubOn :: Ord b => (a -> b) -> [a] -> [a]
        ordNubOn f as = map snd . Map.toList . Map.fromListWith const $ zip (map f as) as

        tryBool :: IO Bool -> IO Bool
        tryBool a = try a >>= \case
            Left (SomeException _) -> pure False
            Right r -> pure r

-- | Execute the given command. Further arguments can be passed in.
--
-- > exe "ls" "-l"
--
-- See also `loadExe` and `loadEnv`.
--
-- NB: It is recommended that you use the template haskell functions to load
-- executables from your path. If you do it manually, it is recommended to
-- use @withFrozenCallStack@ from @GHC.Stack@
--
-- > echo :: Cmd
-- > echo = withFrozenCallStack (exe "echo")
exe :: (Command a, ExecArg str, HasCallStack) => str -> a
exe s = withFrozenCallStack $ toArgs (asArg s)

-- | Create a function for the executable named
loadExe :: ExecReference -> String -> Q [Dec]
loadExe ref s = loadExeAs ref s s

-- | Specify how executables should be referenced.
data ExecReference
    = Absolute -- ^ Find executables on PATH, but store their absolute path
    | SearchPath -- ^ Always search on PATH

-- | Template Haskell function to create a function from a path that will be
-- called. This does not check for executability at compile time.
rawExe :: String -> String -> Q [Dec]
rawExe fnName executable = do
    let
        name = mkName fnName
        impl = valD (varP name) (normalB [|
            withFrozenCallStack $ exe executable
            |]) []
        typ = SigD name (ConT ''Cmd)
    i <- impl
    return [typ,i]

-- | @$(loadExeAs ref fnName executable)@ defines a function called @fnName@
-- which executes the path in @executable@. If @executable@ is an absolute path
-- it is used directly. If it is just an executable name, then it is searched
-- for in the PATH environment variable. If @ref@ is @SearchPath@, the short
-- name is retained, and your PATH will be searched at runtime. If @ref@
-- is @Absolute@, a executable name will be turned into an absolute path, which
-- will be used at runtime.
loadExeAs :: ExecReference -> String -> String -> Q [Dec]
loadExeAs ref fnName executable = do
    -- TODO: Can we place haddock markup in TH generated functions.
    -- TODO: Can we place the man page for each function in there xD
    -- https://ghc.haskell.org/trac/ghc/ticket/5467
    runIO (Dir.findExecutable executable) >>= \case
        Nothing -> error $ "Attempted to load '" ++ executable ++ "', but it is not executable"
        Just absExe ->
            rawExe fnName (case ref of { Absolute -> absExe; SearchPath -> executable })


-- | Takes a string, and makes a Haskell identifier out of it. If the string
-- is a path, the filename portion is used. The exact transformation is that
-- alphanumeric characters are unchanged, @-@ becomes @_@, and @'@ is used to
-- escape all other characters. @_@ becomes @'_@, @.@ becomes @''@ and
-- anthing else is becomes a hex encoded number surrounded by @'@ characters.
--
-- Justification for changing @-@ to @_@ is that @-@ appears far more commonly
-- in executable names than @_@ does, and so we give it the more ergonomic
-- encoding.
--
-- >>> encodeIdentifier "nix-shell"
-- "nix_shell"
--
-- >>> encodeIdentifier "R"
-- "_R"
--
-- >>> encodeIdentifier "x86_64-unknown-linux-gnu-gcc"
-- "x86'_64_unknown_linux_gnu_gcc"
--
-- >>> encodeIdentifier "release.sh"
-- "release''sh"
encodeIdentifier :: String -> String
encodeIdentifier ident =
    let
        fixBody :: String -> String
        fixBody (c:cs)
            | isAlphaNum c = c : fixBody cs
            | c == '-'     = '_' : fixBody cs
            | c == '_'     = '\'' : '_' : fixBody cs
            | c == '.'     = '\'' : '\'' : fixBody cs
            | otherwise    = printf "'%x'%s" (ord c) (fixBody cs)
        fixBody [] = []

        fixStart :: String -> String
        fixStart s@(c : _)
            | isLower c = s
            | otherwise = '_' : s
        fixStart [] = []

        i = fixStart $ fixBody $ takeFileName ident
        -- Includes cd, which has to be a built-in
        reserved = [ "import", "if", "else", "then", "do", "in", "let", "type"
            , "as", "case", "of", "class", "data", "default", "deriving"
            , "instance", "forall", "foreign", "hiding", "infix", "infixl"
            , "infixr", "mdo", "module", "newtype", "proc", "qualified"
            , "rec", "where", "cd"]
    in if i `elem` reserved then i ++ "_" else i


-- | Scans your '$PATH' environment variable and creates a function for each
-- executable found. Binaries that would not create valid Haskell identifiers
-- are encoded using the @'encodeIdentifier'@ function.
loadEnv :: ExecReference -> Q [Dec]
loadEnv ref = loadAnnotatedEnv ref encodeIdentifier

-- | Test to see if an executable can be found either on the $PATH or absolute.
checkExecutable :: FilePath -> IO Bool
checkExecutable = fmap isJust . Dir.findExecutable

-- | Load the given executables into the program, checking their executability
-- and creating a function @missingExecutables@ to do a runtime check for their
-- availability. Uses the @'encodeIdentifier'@ function to create function
-- names.
load :: ExecReference -> [FilePath] -> Q [Dec]
load ref = loadAnnotated ref encodeIdentifier

-- | Same as `load`, but allows you to modify the function names.
loadAnnotated :: ExecReference -> (String -> String) -> [FilePath] -> Q [Dec]
loadAnnotated ref f bins = do
    let pairs = zip (map f bins) bins
    ds <- join <$> mapM (uncurry (loadExeAs ref)) pairs
    d <- valD (varP (mkName "missingExecutables")) (normalB [|
                filterM (fmap not . checkExecutable) bins
            |]) []

    pure (d:ds)

-- | Like `loadEnv`, but allows you to modify the function name that would
-- be generated.
loadAnnotatedEnv :: ExecReference -> (String -> String) -> Q [Dec]
loadAnnotatedEnv ref f = do
    bins <- runIO $ case ref of
        Absolute -> pathBinsAbs
        SearchPath -> pathBins
    i <- forM bins $ \bin -> do
        rawExe (f $ takeFileName bin) bin
    pure (concat i)


-- | Split a string separated by the provided separator. A trailing separator
-- is ignored, and does not produce an empty string. Compatible with the
-- output of most CLI programs, such as @find -print0@.
--
-- >>> endBy "\n" "a\nb\n"
-- ["a","b"]
--
-- >>> endBy "\n" "a\nb"
-- ["a","b"]
--
-- >>> endBy "\n" "a\nb\n\n"
-- ["a","b",""]
endBy :: ByteString -> ByteString -> [ByteString]
endBy s str =
    let splits = Search.split (toStrict s) str
    in dropLastNull splits

    where
        dropLastNull :: [ByteString] -> [ByteString]
        dropLastNull []   = []
        dropLastNull [""] = []
        dropLastNull (a : as) = a : dropLastNull as

-- | Load executables from the given directories
loadFromDirs :: [FilePath] -> Q [Dec]
loadFromDirs ps = loadAnnotatedFromDirs ps encodeIdentifier

-- | Load executables from the given directories appended with @"/bin"@.
--
-- Useful for use with Nix.
loadFromBins :: [FilePath] -> Q [Dec]
loadFromBins = loadFromDirs . fmap (</> "bin")

-- | Load executables from the given dirs, applying the given transformation
-- to the filenames.
loadAnnotatedFromDirs :: [FilePath] -> (String -> String) -> Q [Dec]
loadAnnotatedFromDirs ps f = do
    bins <- runIO $ findBinsIn ps
    i <- forM bins $ \bin -> do
        rawExe (f $ takeFileName bin) bin
    pure (concat i)

-- | Function that splits '\0' separated list of strings. Useful in conjunction
-- with @find . "-print0"@.
endBy0 :: ByteString -> [ByteString]
endBy0 = endBy "\0"

-- | Mimics the shell builtin "cd".
cd' :: FilePath -> IO ()
cd' p = do
    Dir.setCurrentDirectory p
    a <- Dir.getCurrentDirectory
    setEnv "PWD" a

-- | Helper class for variable number of arguments to @cd@ builtin.
class Cd a where
    -- | Mimics the shell builtin "cd". Be careful using this function
    -- in a program, as it doesn't play well with multiple threads. Best
    -- to just use it in an interactive shell or for very simple
    -- transliterations of shell scripts.
    cd :: a

instance (io ~ IO ()) => Cd io where
    cd = getEnv "HOME" >>= cd'

instance {-# OVERLAPS #-} (io ~ IO (), path ~ FilePath) => Cd (path -> io) where
    cd = cd'

-- | @xargs1 n f@ runs @f@ for each item in the input separated by @n@. Similar
-- to the standard @xargs@ utility, but you get to choose the separator, and it
-- only does one argument per command. Compare the following two lines, which
-- do the same thing.
--
-- >>> printf "a\\0b" |> xargs "--null" "-L1" "echo" |> cat
-- a
-- b
-- >>> printf "a\\0b" |> xargs1 "\0" echo |> cat
-- a
-- b
--
-- One benefit of this method over the standard @xargs@ is that we can run
-- Haskell functions as well.
--
-- >>> yes |> head "-n" 5 |> xargs1 "\n" (const $ pure $ Sum 1)
-- Sum {getSum = 5}
xargs1 :: (NFData a, Monoid a) => ByteString -> (ByteString -> Proc a) -> Proc a
xargs1 n f = readInputEndByP n (fmap mconcat . mapM f)

-- | Simple @`Proc`@ that reads its input and can react to the output by
-- calling other @`Proc`@'s which can write something to its stdout.
-- The internal @`Proc`@ is given @/dev/null@ as its input.
readInputP :: (NFData a, Shell io) => (ByteString -> Proc a) -> io a
readInputP f = nativeProc $ \i o e -> do
    s <- hGetContents i
    withNullInput $ \i' ->
        liftIO $ runProc' i' o e (f s)

-- | Like @`readInputP`@, but splits the input.
readInputEndByP :: (NFData a, Shell io) => ByteString -> ([ByteString] -> Proc a) -> io a
readInputEndByP s f = readInputP (f . endBy s)

-- | Like @`readInputP`@, but splits the input on 0 bytes.
readInputEndBy0P :: (NFData a, Shell io) => ([ByteString] -> Proc a) -> io a
readInputEndBy0P = readInputEndByP "\0"

-- | Like @`readInputP`@, but splits the input on new lines.
readInputLinesP :: (NFData a, Shell io) => ([ByteString] -> Proc a) -> io a
readInputLinesP = readInputEndByP "\n"

-- | Create a null file handle.
withNullInput :: (Handle -> IO a) -> IO a
withNullInput = withFile "/dev/null" ReadMode

-- | Bracket a @`hDup`@
withDuplicate :: Handle -> (Handle -> IO a) -> IO a
withDuplicate h = bracket (hDup h) hClose

-- | Bracket three @`hDup`@s
withDuplicates :: Handle -> Handle -> Handle -> (Handle -> Handle -> Handle -> IO a) -> IO a
withDuplicates a b c f =
    withDuplicate a $ \a' -> withDuplicate b $ \b' -> withDuplicate c $ \c' -> f a' b' c'

-- | Bracket two @`hDup`@s and provide a null input handle.
withDuplicateNullInput :: Handle -> Handle -> (Handle -> Handle -> Handle -> IO a) -> IO a
withDuplicateNullInput a b f = do
    withNullInput $ \i -> do
        withDuplicate a $ \a' -> withDuplicate b $ \b' -> f i a' b'

-- | Duplicate a @`Handle`@ without trying to flush buffers. Only works on @`FileHandle`@s.
--
-- hDuplicate tries to "flush" read buffers by seeking backwards, which doesn't
-- work for streams/pipes. Since we are simulating a @fork + exec@ in @`nativeProc`@,
-- losing the buffers is actually the expected behaviour. (System.Process doesn't
-- attempt to flush the buffers).
--
-- NB: An alternate solution that we could implement (even for System.Process forks)
-- is to create a fresh pipe and spawn an async task to forward buffered content
-- from the original handle if there is something in the buffer. My concern would
-- be that it might be a performance hit that people aren't expecting.
--
-- Code basically copied from
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.IO.Handle.html#hDuplicate
-- with minor modifications.
hDup :: Handle -> IO Handle
hDup h@(FileHandle path m) = do
    withHandle_' "hDup" h m $ \h_ ->
        dupHandleShh path h Nothing h_ (Just handleFinalizer)
hDup h@(DuplexHandle path r w) = do
    (FileHandle _ write_m) <-
        withHandle_' "hDup" h w $ \h_ ->
            dupHandleShh path h Nothing h_ (Just handleFinalizer)
    (FileHandle _ read_m) <-
        withHandle_' "hDup" h r $ \h_ ->
            dupHandleShh path h (Just write_m) h_  Nothing
    return (DuplexHandle path read_m write_m)

-- | Helper function for duplicating a Handle
dupHandleShh
    :: FilePath
    -> Handle
    -> Maybe (MVar Handle__)
    -> Handle__
    -> Maybe HandleFinalizer
    -> IO Handle
dupHandleShh filepath h other_side h_@Handle__{..} mb_finalizer = do
    case other_side of
        Nothing -> do
            new_dev <- IODevice.dup haDevice
            dupHandleShh_ new_dev filepath other_side h_ mb_finalizer
        Just r  ->
            withHandle_' "dupHandleShh" h r $ \Handle__{haDevice=dev} -> do
                dupHandleShh_ dev filepath other_side h_ mb_finalizer

-- | Helper function for duplicating a Handle
dupHandleShh_
#if __GLASGOW_HASKELL__ < 900
    :: (IODevice dev, BufferedIO dev, Typeable dev) => dev
#else
    :: (RawIO dev, IODevice dev, BufferedIO dev, Typeable dev) => dev
#endif
    -> FilePath
    -> Maybe (MVar Handle__)
    -> Handle__
    -> Maybe HandleFinalizer
    -> IO Handle
dupHandleShh_ new_dev filepath other_side Handle__{..} mb_finalizer = do
    -- XXX wrong!
    mb_codec <- if isJust haEncoder then fmap Just getLocaleEncoding else return Nothing
    mkHandle new_dev filepath haType True{-buffered-} mb_codec
        NewlineMode { inputNL = haInputNL, outputNL = haOutputNL }
        mb_finalizer other_side
