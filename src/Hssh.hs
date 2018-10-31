{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Hssh where

import System.IO.Unsafe

import System.Exit
import System.Posix.Signals
import System.IO.Error
import GHC.IO.Exception
import Foreign.C
import Control.Monad.Trans.Free
import Data.Functor.Coyoneda
import System.IO
import System.Process
import Control.Monad.Writer
import Data.List
import Data.List.Split
import Data.Char
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.State
import System.Directory
import Language.Haskell.TH
import System.Environment
import Control.Exception as C
import Control.Concurrent.MVar
import Control.Concurrent
import Control.DeepSeq (rnf)
import Control.Concurrent.Async

-- | This function needs to be called in order to use the library succesfully
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
    { prog :: String
    , args :: [String]
    , code :: Int
    } deriving (Eq, Ord)

instance Show Failure where
    show f = concat $
        [ "Command `"
        ]
        ++ [intercalate " " (prog f : map show (args f))]
        ++
        [ "` failed [exit "
        , show (code f)
        , "]"
        ]

instance Exception Failure

class PipeResult f where
    -- | Use this to send the output of on process into the input of another.
    -- This is just like a shells `|` operator.
    --
    -- The result is polymorphic in it's output, and can result in either
    -- another `Proc a` or an `IO a` depending on the context in which it is
    -- used.
    (|>) :: Proc a -> Proc a -> f a

    -- | Similar to @|!>@ except that it connects stderr to stdin of the
    -- next process in the chain.
    --
    -- NB: The next command to be @|>@ on will recapture the stdout of
    -- both preceding processes, because they are both going to the same
    -- handle!
    --                                            
    -- This is probably not what you want, see the @&>@ and @&!>@ operators
    -- for redirection.
    (|!>) :: Proc a -> Proc a -> f a

    -- Redirect StdOut of this process to another location
    --
    -- > ls &> Append "/dev/null"
    (&>) :: Proc a -> Stream -> f a

    -- Redirect StdErr of this process to another location
    (&!>) :: Proc a -> Stream -> f a

(<|) :: PipeResult f => Proc a -> Proc a -> f a
(<|) = flip (|>)

instance PipeResult IO where
    a |> b = runProc $ a |> b
    a |!> b = runProc $ a |!> b
    a &> s = runProc $ a &> s
    a &!> s = runProc $ a &!> s

withPipe :: (Handle -> Handle -> IO a) -> IO a
withPipe k =
    bracket
        createPipe
        (\(r,w) -> hClose r `finally` hClose w)
        (\(r,w) -> k r w)

instance PipeResult Proc where
    (Proc a) |> (Proc b) = Proc $ \i o e pl pw ->
        withPipe $ \r w -> do
            a' <- async $ a i w e (pure ()) (hClose w)
            b' <- async $ b r o e (pure ()) (hClose r)
            link2 a' b'
            (_, br) <- (pl >> waitBoth a' b') `finally` pw
            pure br

    (Proc a) |!> (Proc b) = Proc $ \i o e pl pw -> do
        withPipe $ \r w -> do
            a' <- async $ a i o w (pure ()) (hClose w)
            b' <- async $ b r o e (pure ()) (hClose r)
            link2 a' b'
            (_, br) <- (pl >> waitBoth a' b') `finally` pw
            pure br

    p &> StdOut = p
    (Proc f) &> StdErr = Proc $ \i o e pl pw -> f i e e pl pw
    (Proc f) &> (Truncate path) = Proc $ \i o e pl pw ->
        withBinaryFile path WriteMode $ \h -> f i h e pl pw
    (Proc f) &> (Append path) = Proc $ \i o e pl pw ->
        withBinaryFile path AppendMode $ \h -> f i h e pl pw

    p &!> StdErr = p
    (Proc f) &!> StdOut = Proc $ \i o e pl pw -> f i o o pl pw
    (Proc f) &!> (Truncate path) = Proc $ \i o e pl pw ->
        withBinaryFile path WriteMode $ \h -> f i o h pl pw
    (Proc f) &!> (Append path) = Proc $ \i o e pl pw ->
        withBinaryFile path AppendMode $ \h -> f i o h pl pw
    

data Stream = StdOut | StdErr | Truncate FilePath | Append FilePath

newtype Proc a = Proc (Handle -> Handle -> Handle -> IO () -> IO () -> IO a)
    deriving Functor

-- | The @Semigroup@ instance for @Proc@ pipes the stdout of one process
-- into the stdin of the next. However, consider using `|>` instead which
-- behaves when used in an @IO@ context. If you use `<>` in an IO monad
-- you will be using the `IO` instance of semigroup which is a sequential
-- execution. @|>@ prevents that error.
instance Semigroup (Proc a) where
    (<>) = (|>)

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

runProc :: Proc a -> IO a
runProc (Proc f) = f stdin stdout stderr (pure ()) (pure ())

mkProc :: String -> [String] -> Proc ()
mkProc cmd args = Proc $ \i o e pl pw -> do
    bracket
        (createProcess_ cmd (proc cmd args)
            { std_in = UseHandle i
            , std_out = UseHandle o
            , std_err = UseHandle e
            , close_fds = True
            }
        )
        (\(_,_,_,ph) -> terminateProcess ph)
        $ \(_,_,_,ph) -> do
            pl
            (waitProc cmd args ph `onException` terminateProcess ph) `finally` pw


-- | Read the stdout of a @Proc@ in any @MonadIO@ (including other @Proc@s).
-- This is strict, so the whole output is read into a @String@. See @withRead@
-- for a lazy version that can be used for streaming.
readProc :: MonadIO io => Proc a -> io String
readProc (Proc f) = liftIO $ do
    (r,w) <- createPipe
    (_,o) <- concurrently
        (f stdin w stderr (pure ()) (hClose w))
        (do
            output <- hGetContents r
            C.evaluate $ rnf output
            hClose r
            pure output
        )
    pure o

-- | Run a process and capture it's output lazily. Once the continuation
-- is completed, the handles are closed, and the process is terminated.
withRead :: MonadIO io => Proc a -> (String -> IO b) -> io b
withRead (Proc f) k = liftIO $ 
    withPipe $ \r w -> do
        withAsync (f stdin w stderr (pure ()) (hClose w)) $ \_ ->
            (hGetContents r >>= k) `finally` hClose r


readWriteProc :: MonadIO io => Proc a -> String -> io String
readWriteProc (Proc f) input = liftIO $ do
    (ri,wi) <- createPipe
    (ro,wo) <- createPipe
    (_,o) <- concurrently
        (concurrently
            (f ri wo stderr (pure ()) (hClose wo `finally` hClose ri))
            (hPutStr wi input `finally` hClose wi)
        ) (do
            output <- hGetContents ro
            C.evaluate $ rnf output
            hClose ro
            pure output
        )
    pure o

writeProc :: MonadIO io => Proc a -> String -> io a
writeProc (Proc f) input = liftIO $ do
    (r,w) <- createPipe
    fst <$> concurrently
        (f r stdout stderr (pure ()) (hClose r))
        (hPutStr w input `finally` hClose w)

waitProc :: String -> [String] -> ProcessHandle -> IO ()
waitProc cmd arg ph = waitForProcess ph >>= \case
    ExitFailure c
        | fromIntegral c == negate sigPIPE -> pure ()
        | otherwise -> throwIO $ createFailure cmd arg c
    ExitSuccess -> pure ()

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

catchFailure :: IO a -> IO (Either Failure a)
catchFailure = try

catchCode :: IO a -> IO Int
catchCode a = catchFailure a >>= \case
    Right _ -> pure 0
    Left f  -> pure $ code f


readTrim :: MonadIO io => Proc a -> io String
readTrim = fmap trim . readProc


(>>>) :: MonadIO io => String -> Proc a -> io a
(>>>) = flip writeProc

(<<<) :: MonadIO io => Proc a -> String -> io a
(<<<) = writeProc

-- | A class for things that can be converted to arguments on the command
-- line. The default implementation is to use `show`.
class ExecArg a where
    asArg :: a -> String
    default asArg :: Show a => a -> String
    asArg = show

instance ExecArg String where
    asArg = id

instance ExecArg Int
instance ExecArg Integer
instance ExecArg Word

-- | A class for building up a command
class ExecArgs a where
    toArgs :: [String] -> a

instance ExecArgs (Proc ()) where
    toArgs (cmd:args) = mkProc cmd args

instance (ExecArg b, ExecArgs a) => ExecArgs (b -> a) where
    toArgs f i = toArgs $ f ++ [asArg i]
-- 
-- instance ExecArgs [String] where
--     toArgs = id

-- | Commands can be executed directly in IO (this goes via the @CmdT@ instance)
instance ExecArgs (IO ()) where
    toArgs = runProc . toArgs

-- | Force a `()` result.
class Unit a
instance {-# OVERLAPPING #-} Unit b => Unit (a -> b)
instance {-# OVERLAPPABLE #-} a ~ () => Unit (m a)
-- instance {-# OVERLAPPABLE #-} Unit Pipe

-- | Get all files in a directory on your `$PATH`.
--
-- TODO: Check for executability.
pathBins :: IO [FilePath]
pathBins = do
    paths <- splitOn ":" <$> getEnv "PATH"
    paths <- filterM doesDirectoryExist paths
    bins <- nub . concat <$> mapM getDirectoryContents paths
    return $ flip filter bins $ \p -> all isLower p && not (p `elem` ["import", "if", "else", "then", "do", "in", "let", "type"])

-- | Create a function for the executable named
loadExe :: String -> Q [Dec]
loadExe exe =
    let
        impl = valD (varP (mkName exe)) (normalB [|
            toArgs [] exe 
            |]) []
        name = mkName exe
        typn = mkName "a"
        typ = SigD (mkName exe) (ForallT [PlainTV typn] [AppT (ConT ''Unit) (VarT typn), AppT (ConT ''ExecArgs) (VarT typn)] (VarT typn))
    in do
        i <- impl
        return $ [typ,i]

-- | Scans your '$PATH' environment variable and creates a function for each
-- executable found.
loadEnv :: Q [Dec]
loadEnv = do
    bins <- runIO pathBins
    fmap join $ mapM loadExe bins

createFailure :: String -> [String] -> Int -> Failure
createFailure cmd args i = Failure cmd args i

-- TODO: Does this exist anywhere?
-- | Helper type for building a monad.
data U f a where
    U :: f -> U f ()


-- | Function that splits '\0' seperated list of strings.
split0 :: String -> [String]
split0 = endBy "\0"

-- | A convinience function for reading in a @"\NUL"@ seperated list of
-- strings. This is commonly used when dealing with paths.
--
-- ```
-- readSplit0 $ find "-print0"
-- ```
readSplit0 :: Proc () -> IO [String]
readSplit0 p = split0 <$> readProc p

-- | A convinience function for reading the output lines of a @Proc@.
readLines :: Proc () -> IO [String]
readLines p = lines <$> readProc p

readAuto :: Read a => Proc () -> IO a
readAuto p = read <$> readProc p
