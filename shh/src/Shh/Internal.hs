{-# LANGUAGE FlexibleInstances #-}
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

-- | See documentation for "Shh".
module Shh.Internal where

import GHC.IO.Exception
import GHC.IO.Handle
import System.IO.Error

import Control.Concurrent.Async
import Control.DeepSeq (force,NFData)
import Control.Exception as C
import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isLower, isSpace, isAlphaNum, isUpper, toLower, isNumber)
import Data.List (dropWhileEnd, intercalate)
import Data.List.Split (endBy, splitOn)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Language.Haskell.TH
import qualified System.Directory as Dir
import System.Environment (getEnv, setEnv)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName)
import System.IO
import System.Posix.Signals
import System.Process

-- | This function needs to be called in order to use the library successfully
-- from GHCi.
initInteractive :: IO ()
initInteractive = do
    hSetBuffering stdin LineBuffering

-- | When a process exits with a non-zero exit code
-- we throw this Failure exception.
--
-- The only exception to this is when a process is terminated
-- by SIGPIPE in a pipeline, in which case we ignore it.
data Failure = Failure
    { failureProg :: String
    , failureArgs :: [String]
    , failureCode :: Int
    } deriving (Eq, Ord)

instance Show Failure where
    show f = concat $
        [ "Command `"
        ]
        ++ [intercalate " " (failureProg f : map show (failureArgs f))]
        ++
        [ "` failed [exit "
        , show (failureCode f)
        , "]"
        ]

instance Exception Failure

-- | This class is used to allow most of the operators in Shh to be
-- polymorphic in their return value. This makes using them in an `IO`
-- context easier (we can avoid having to prepend everything with a
-- `runProc`).
class PipeResult f where
    -- | Use this to send the output of on process into the input of another.
    -- This is just like a shells `|` operator.
    --
    -- The result is polymorphic in it's output, and can result in either
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
    (|>) :: Proc b -> Proc a -> f a
    infixl 1 |>

    -- | Similar to `|!>` except that it connects stderr to stdin of the
    -- next process in the chain.
    --
    -- NB: The next command to be `|>` on will recapture the stdout of
    -- both preceding processes, because they are both going to the same
    -- handle!
    --                                            
    -- This is probably not what you want, see the `&>` and `&!>` operators
    -- for redirection.
    (|!>) :: Proc b -> Proc a -> f a
    infixl 1 |!>

    -- | Redirect stdout of this process to another location
    --
    -- > ls &> Append "/dev/null"
    (&>) :: Proc a -> Stream -> f a
    infixl 9 &>

    -- | Redirect stderr of this process to another location
    --
    -- > ls &!> StdOut
    (&!>) :: Proc a -> Stream -> f a
    infixl 9 &!>

    -- | Lift a Haskell function into a `Proc`.
    nativeProc :: NFData a => (Handle -> Handle -> Handle -> IO a) -> f a

-- | Flipped version of `|>`
(<|) :: PipeResult f => Proc a -> Proc b -> f a
(<|) = flip (|>)
infixr 1 <|

instance PipeResult IO where
    a |> b = runProc $ a |> b
    a |!> b = runProc $ a |!> b
    a &> s = runProc $ a &> s
    a &!> s = runProc $ a &!> s
    nativeProc f = runProc $ nativeProc f

-- | Create a pipe, and close both ends on exception.
withPipe :: (Handle -> Handle -> IO a) -> IO a
withPipe k =
    bracket
        createPipe
        (\(r,w) -> hClose r `finally` hClose w)
        (\(r,w) -> k r w)

instance PipeResult Proc where
    (Proc a) |> (Proc b) = Proc $ \i o e pl pw ->
        withPipe $ \r w -> do
            let
                a' = a i w e (pure ()) (hClose w)
                b' = b r o e (pure ()) (hClose r)
            (_, br) <- (pl >> concurrently a' b') `finally` pw
            pure br

    (Proc a) |!> (Proc b) = Proc $ \i o e pl pw -> do
        withPipe $ \r w -> do
            let
                a' = a i o w (pure ()) (hClose w)
                b' = b r o e (pure ()) (hClose r)
            (_, br) <- (pl >> concurrently a' b') `finally` pw
            pure br

    p &> StdOut = p
    (Proc f) &> StdErr = Proc $ \i _ e pl pw -> f i e e pl pw
    (Proc f) &> (Truncate path) = Proc $ \i _ e pl pw ->
        withBinaryFile path WriteMode $ \h -> f i h e pl pw
    (Proc f) &> (Append path) = Proc $ \i _ e pl pw ->
        withBinaryFile path AppendMode $ \h -> f i h e pl pw

    p &!> StdErr = p
    (Proc f) &!> StdOut = Proc $ \i o _ pl pw -> f i o o pl pw
    (Proc f) &!> (Truncate path) = Proc $ \i o _ pl pw ->
        withBinaryFile path WriteMode $ \h -> f i o h pl pw
    (Proc f) &!> (Append path) = Proc $ \i o _ pl pw ->
        withBinaryFile path AppendMode $ \h -> f i o h pl pw

    nativeProc f = Proc $ \i o e pl pw -> handle handler $ do
        pl
        -- We duplicate these so that you can't accidentally close the
        -- real ones.
        i' <- hDuplicate i
        o' <- hDuplicate o
        e' <- hDuplicate e

        -- We NEVER want the process to be able to close stderr because it
        -- is shared by the whole pipeline.
        -- e' <- hDuplicate e

        (f i' o' e' >>= C.evaluate . force)
            `finally` (hClose i')
            `finally` (hClose o')
            `finally` (hClose e')
            `finally` pw

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

-- | Simple @`Proc`@ that writes a `String` to it's @stdout@. This behaves
-- very much like the standard @echo@ utility, except that there is no
-- restriction as to what can be in the string argument.
writeOutput :: PipeResult io => String -> io ()
writeOutput s = nativeProc $ \_ o _ -> do
    hPutStr o s

-- | Simple @`Proc`@ that writes a `String` to it's @stderr@.
-- See also @`writeOutput`@.
writeError :: PipeResult io => String -> io ()
writeError s = nativeProc $ \_ _ e -> do
    hPutStr e s

-- | Simple @`Proc`@ that reads it's input, and can react to it with an IO
-- action. Does not write anything to it's output. See also @`capture`@.
readInput :: (NFData a, PipeResult io) => (String -> IO a) -> io a
readInput f = nativeProc $ \i _ _ -> do
    hGetContents i >>= f

-- | Creates a pure @`Proc`@ that simple transforms the @stdin@ and writes
-- it to @stdout@.
pureProc :: PipeResult io => (String -> String) -> io ()
pureProc f = nativeProc $ \i o _ -> do
    s <- hGetContents i
    hPutStr o (f s)

-- | Captures the stdout of a process and prefixes all the lines with
-- the given string.
--
-- some_command |> prefixLines "stdout: " |!> prefixLines "stderr: " &> StdErr
prefixLines :: PipeResult io => String -> io ()
prefixLines s = pureProc $ unlines . map (s ++) . lines

-- | Provide the stdin of a `Proc` from a `String`
writeProc :: PipeResult io => Proc a -> String -> io a
writeProc p s = writeOutput s |> p

-- | Run a process and capture it's output lazily. Once the continuation
-- is completed, the handles are closed. However, the process is run
-- until it naturally terminates in order to capture the correct exit
-- code. Most utilities behave correctly with this (e.g. @cat@ will
-- terminate if you close the handle).
withRead :: (PipeResult f, NFData b) => Proc a -> (String -> IO b) -> f b
withRead p f = p |> readInput f

-- | Type used to represent destinations for redirects. @`Truncate` file@
-- is like @> file@ in a shell, and @`Append` file@ is like @>> file@.
data Stream = StdOut | StdErr | Truncate FilePath | Append FilePath

-- | Shortcut for @`Truncate` "\/dev\/null"@
devNull :: Stream
devNull = Truncate "/dev/null"

-- | Type representing a series or pipeline (or both) of shell commands.
--
-- @Proc@'s can communicate to each other via @stdin@, @stdout@ and @stderr@
-- and can communicate to Haskell via their parameterised return type, or by
-- throwing an exception.
newtype Proc a = Proc (Handle -> Handle -> Handle -> IO () -> IO () -> IO a)
    deriving Functor

instance MonadIO Proc where
    liftIO a = Proc $ \_ _ _ pl pw -> do
        (pl >> a) `finally` pw

-- | The `Semigroup` instance for `Proc` pipes the stdout of one process
-- into the stdin of the next. However, consider using `|>` instead which
-- behaves when used in an `IO` context. If you use `<>` in an IO monad
-- you will be using the `IO` instance of semigroup which is a sequential
-- execution. `|>` prevents that error.
instance Semigroup (Proc a) where
    (<>) = (|>)

instance (a ~ ()) => Monoid (Proc a) where
    mempty = Proc $ \_ _ _ pl pw -> pl `finally` pw

instance Applicative Proc where
    pure a = Proc $ \_ _ _ pw pl -> do
        pw `finally` pl
        pure a

    f <*> a = do
        f' <- f
        a' <- a
        pure (f' a')
        

instance Monad Proc where
    (Proc a) >>= f = Proc $ \i o e pl pw -> do
        ar <- a i o e pl (pure ())
        let
            Proc f' = f ar
        f' i o e (pure ()) pw

-- | Run's a `Proc` in `IO`. This is usually not required, as most
-- commands in Shh are polymorphic in their return type, and work
-- just fine in `IO` directly.
runProc :: Proc a -> IO a
runProc = runProc' stdin stdout stderr

-- | Run's a `Proc` in `IO`. Like `runProc`, but you get to choose the handles.
runProc' :: Handle -> Handle -> Handle -> Proc a -> IO a
runProc' i o e (Proc f) = do
    r <- f i o e (pure ()) (pure ())
    -- Evaluate to WHNF to uncover any ResourceVanished exceptions
    -- that may be hiding in there from `nativeProc`. These should
    -- not happen under normal circumstances, but we would at least
    -- like to have the exception thrown ASAP if, for whatever reason,
    -- it does happen.
    pure $! r

-- | Create a `Proc` from a command and a list of arguments.
-- The boolean represents whether we should delegate control-c
-- or not. Most uses of @`mkProc'`@ in Shh do not delegate control-c.
mkProc' :: Bool -> String -> [String] -> Proc ()
mkProc' delegate cmd args = Proc $ \i o e pl pw -> do
    bracket
        (createProcess_ cmd (proc cmd args)
            { std_in = UseHandle i
            , std_out = UseHandle o
            , std_err = UseHandle e
            , close_fds = True
            , delegate_ctlc = delegate
            }
        )
        (\(_,_,_,ph) -> terminateProcess ph)
        $ \(_,_,_,ph) -> do
            pl
            (waitProc cmd args ph `onException` terminateProcess ph) `finally` pw

-- | Create a `Proc` from a command and a list of arguments. Does not delegate
-- control-c handling.
mkProc :: String -> [String] -> Proc ()
mkProc = mkProc' False

-- | Read the stdout of a `Proc`. This captures stdout, so further piping will
-- not see anything on the input.
--
-- This is strict, so the whole output is read into a `String`. See `withRead`
-- for a lazy version that can be used for streaming.
readProc :: PipeResult io => Proc a -> io String
readProc p = withRead p pure

-- | A special `Proc` which captures it's stdin and presents it as a `String`
-- to Haskell.
--
-- >>> printf "Hello" |> shasum |> capture
-- "f7ff9e8b7bb2e09b70935a5d785e0cc5d9d0abf0  -\n"
capture :: PipeResult io => io String
capture = readInput pure

-- | Apply a transformation function to the string before the IO action.
withRead' :: (NFData b, PipeResult io) => (String -> a) -> Proc x -> (a -> IO b) -> io b
withRead' f p io = withRead p (io . f)

-- | Like @'withRead'@ except it splits the string with @'split0'@ first.
withReadSplit0 :: (NFData b, PipeResult io) => Proc a -> ([String] -> IO b) -> io b
withReadSplit0 = withRead' split0

-- | Like @'withRead'@ except it splits the string with @'lines'@ first.
--
-- NB: Please consider using @'withReadSplit0'@ where you can.
withReadLines :: (NFData b, PipeResult io) => Proc a -> ([String] -> IO b) -> io b
withReadLines = withRead' lines

-- | Like @'withRead'@ except it splits the string with @'words'@ first.
withReadWords :: (NFData b, PipeResult io) => Proc a -> ([String] -> IO b) -> io b
withReadWords = withRead' words

-- | Read and write to a `Proc`. Same as
-- @readProc proc <<< input@
readWriteProc :: MonadIO io => Proc a -> String -> io String
readWriteProc p input = liftIO $ readProc p <<< input

-- | Some as `readWriteProc`. Apply a `Proc` to a `String`.
--
-- >>> apply shasum "Hello"
-- "f7ff9e8b7bb2e09b70935a5d785e0cc5d9d0abf0  -\n"
apply :: MonadIO io => Proc a -> String -> io String
apply = readWriteProc

-- | Flipped, infix version of `writeProc`
(>>>) :: PipeResult io => String -> Proc a -> io a
(>>>) = flip writeProc


-- | Infix version of `writeProc`
(<<<) :: PipeResult io => Proc a -> String -> io a
(<<<) = writeProc

-- | Wait on a given `ProcessHandle`, and throw an exception of
-- type `Failure` if it's exit code is non-zero (ignoring SIGPIPE)
waitProc :: String -> [String] -> ProcessHandle -> IO ()
waitProc cmd arg ph = waitForProcess ph >>= \case
    ExitFailure c
        | fromIntegral c == negate sigPIPE -> pure ()
        | otherwise -> throwIO $ Failure cmd arg c
    ExitSuccess -> pure ()

-- | Trim leading and tailing whitespace.
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Allow us to catch `Failure` exceptions in `IO` and `Proc`
class ProcFailure m where
    -- | Run a `Proc` action, catching an `Failure` exceptions
    -- and returning them.
    catchFailure :: Proc a -> m (Either Failure a)

instance ProcFailure Proc where
    catchFailure (Proc f) = Proc $ \i o e pl pw -> do
        try $ f i o e pl pw

instance ProcFailure IO where
    catchFailure = runProc . catchFailure

-- | Run a `Proc` action, ignoring any `Failure` exceptions.
-- This can be used to prevent a process from interrupting a whole pipeline.
--
-- >>> false `|>` (sleep 2 >> echo 1)
-- *** Exception: Command `false` failed [exit 1]
--
-- >>> (ignoreFailure  false) `|>` (sleep 2 >> echo 1)
-- 1
ignoreFailure :: (Functor m, ProcFailure m) => Proc a -> m ()
ignoreFailure = void . catchFailure

-- | Run an `Proc` action returning the return code if an
-- exception was thrown, and 0 if it wasn't.
catchCode :: (Functor m, ProcFailure m) => Proc a -> m Int
catchCode = fmap getCode . catchFailure
    where
        getCode (Right _) = 0
        getCode (Left  f) = failureCode f

-- | Like `readProc`, but trim leading and tailing whitespace.
readTrim :: (Functor io, PipeResult io) => Proc a -> io String
readTrim = fmap trim . readProc

-- | A class for things that can be converted to arguments on the command
-- line. The default implementation is to use `show`.
class ExecArg a where
    asArg :: a -> [String]
    default asArg :: Show a => a -> [String]
    asArg a = [show a]

    -- God, I hate that String is [Char]...
    asArgFromList :: [a] -> [String]
    default asArgFromList :: Show a => [a] -> [String]
    asArgFromList = concatMap asArg

instance ExecArg Char where
    asArg s = [[s]]
    asArgFromList s = [s]

instance ExecArg a => ExecArg [a] where
    asArg = asArgFromList
    asArgFromList = concatMap asArg

instance ExecArg Int
instance ExecArg Integer
instance ExecArg Word

-- | A class for building up a command
class ExecArgs a where
    toArgs :: [String] -> a

instance ExecArgs (Proc ()) where
    toArgs (cmd:args) = mkProc cmd args
    toArgs _ = error "The impossible happened. How did you construct this?"

instance (ExecArg b, ExecArgs a) => ExecArgs (b -> a) where
    toArgs f i = toArgs $ f ++ asArg i

-- | Commands can be executed directly in IO
instance ExecArgs (IO ()) where
    toArgs = runProc . toArgs

-- | Force a `()` result.
class Unit a
instance {-# OVERLAPPING #-} Unit b => Unit (a -> b)
instance {-# OVERLAPPABLE #-} a ~ () => Unit (m a)

-- | Get all executables on your `$PATH`.
pathBins :: IO [FilePath]
pathBins = map takeFileName <$> pathBinsAbs

-- | Get all uniquely named executables on your `$PATH` as absolute
-- file names. The uniqueness is determined by the filename, and not
-- the whole path. First one found wins.
pathBinsAbs :: IO [FilePath]
pathBinsAbs = do
    pathsVar <- splitOn ":" <$> getEnv "PATH"
    paths <- filterM Dir.doesDirectoryExist pathsVar
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
exe :: (Unit a, ExecArgs a) => String -> a
exe s = toArgs [s]

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
        name = mkName $ fnName
        impl = valD (varP name) (normalB [|
            exe executable
            |]) []
        typn = mkName "a"
        typ = SigD name (ForallT [PlainTV typn] [AppT (ConT ''Unit) (VarT typn), AppT (ConT ''ExecArgs) (VarT typn)] (VarT typn))
    i <- impl
    return $ [typ,i]

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

-- | Takes a string, and makes a Haskell identifier out of it. There
-- is some chance of overlap. If the string is a path, the filename portion
-- is used. The transformation replaces all non-alphanumeric characters
-- with @'_'@. If the first character is uppercase it is forced into lowercase.
-- If it starts with a number, it is prefixed with `_`. If it overlaps with
-- a reserved word or a builtin, it is suffixed with an `_`.
encodeIdentifier :: String -> String
encodeIdentifier ident =
    let
        i = go (takeFileName ident)
        go (c:cs)
            | isLower  c = c : go' cs
            | isUpper  c = toLower c : go' cs
            | isNumber c = '_' : go' (c : cs)
            | otherwise  = go' (c:cs)
        go [] = "_"
        go' (c:cs)
            | isAlphaNum c = c : go' cs
            | otherwise    = '_' : go' cs
        go' [] = []
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
load :: ExecReference -> [String] -> Q [Dec]
load ref = loadAnnotated ref encodeIdentifier

-- | Same as `load`, but allows you to modify the function names.
loadAnnotated :: ExecReference -> (String -> String) -> [String] -> Q [Dec]
loadAnnotated ref f bins = do
    let pairs = zip (map f bins) bins
    ds <- fmap join $ mapM (uncurry (loadExeAs ref)) pairs
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

-- | Function that splits '\0' separated list of strings. Useful in conjunction
-- with @find . "-print0"@.
split0 :: String -> [String]
split0 = endBy "\0"

-- | A convenience function for reading in a @"\\NUL"@ separated list of
-- strings. This is commonly used when dealing with paths.
--
-- > readSplit0 $ find "-print0"
readSplit0 :: Proc () -> IO [String]
readSplit0 p = withReadSplit0 p pure

-- | A convenience function for reading the output lines of a `Proc`.
--
-- Note: Please consider using @'readSplit0'@ instead if you can.
readLines :: Proc () -> IO [String]
readLines p = withReadLines p pure

-- | Read output into a list of words
readWords :: Proc () -> IO [String]
readWords p = withReadWords p pure

-- | Like `readProc`, but attempts to `Prelude.read` the result.
readAuto :: Read a => Proc () -> IO a
readAuto p = read <$> readProc p

-- | Mimics the shell builtin "cd".
cd' :: FilePath -> IO ()
cd' p = do
    Dir.setCurrentDirectory p
    a <- Dir.getCurrentDirectory
    setEnv "PWD" a

-- | Helper class for variable number of arguments to @cd@ builtin.
class Cd a where
    -- | Mimics the shell builtin "cd"
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
-- >>> xargs "--null" "-L1" "echo" -- Using standard xargs utility.
-- >>> xargs1 "\0" echo
--
-- One benefit of this method over the standard @xargs@ is that we can run
-- Haskell functions as well.
--
-- >>> find "." "-print0" |> xargs1 "\0" (const $ pure $ Sum 1)
-- Sum {getSum = 1590}
xargs1 :: (NFData a, Monoid a) => String -> (String -> Proc a) -> Proc a
xargs1 n f = nativeProc $ \i o e -> do
    ls <- endBy n <$> hGetContents i
    liftIO $ runProc' i o e $ mconcat <$> mapM f ls
