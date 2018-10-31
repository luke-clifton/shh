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

instance PipeResult Proc where
    (|>) = (<>)
    (PP a) |!> (PP b) = PP $ \i o e -> do
        (r,w) <- createPipe
        let
            run = do
                aw <- a i o w
                bw <- b r o e
                pure $ snd <$> concurrently aw bw
        run `onException` (hClose r `finally` hClose w)


    (PP f) &> StdIO = PP $ \i o e -> f i o e
    (PP f) &> StdErr = PP $ \i o e -> f i e e
    (PP f) &> (Truncate fp) = PP $ \i o e -> do
        withBinaryFile fp WriteMode $ \h -> do
            f i h e
    (PP f) &> (Append fp) = PP $ \i o e -> do
        withBinaryFile fp AppendMode $ \h -> do
            f i h e

    (PP f) &!> StdIO = PP $ \i o e -> f i o o
    (PP f) &!> StdErr = PP $ \i o e -> f i o e

    (PP f) &!> (Truncate fp) = PP $ \i o e -> do
        withBinaryFile fp WriteMode $ \h -> do
            f i o h
    (PP f) &!> (Append fp) = PP $ \i o e -> do
        withBinaryFile fp AppendMode $ \h -> do
            f i o h

redirect :: Proc a -> Proc a
redirect (PP f) = PP $ \i o e -> f i o o

data Stream = StdIO | StdErr | Truncate FilePath | Append FilePath


-- It is very important that the returned IO action is actually run, in
-- order to prevent leaked resources.
newtype Proc a = PP (Handle -> Handle -> Handle -> IO (IO a))
    deriving Functor

newtype PrXX a = PrXX (Handle -> Handle -> Handle -> IO () -> IO () -> IO a)
    deriving Functor

instance Semigroup (PrXX a) where
    (PrXX a) <> (PrXX b) = PrXX $ \i o e pl pw -> do
        (r,w) <- createPipe
        a' <- async $ a i w e (pure ()) (hClose w)
        b' <- async $ b r o e (pure ()) (hClose r)
        (_, br) <- (pl >> waitBoth a' b') `finally` pw
        pure br

instance Applicative PrXX where
    pure a = PrXX $ \_ _ _ pw pl -> do
        pw `finally` pl
        pure a

    f <*> a = do
        f' <- f
        a' <- a
        pure (f' a')
        

instance Monad PrXX where
    (PrXX a) >>= f = PrXX $ \i o e pl pw -> do
        ar <- a i o e pl (pure ())
        let
            PrXX f' = f ar
        f' i o e (pure ()) pw

runPrXX :: PrXX a -> IO a
runPrXX (PrXX f) = f stdin stdout stderr (pure ()) (pure ())

mkPrXX :: String -> [String] -> PrXX ()
mkPrXX cmd args = PrXX $ \i o e pl pw -> do
    (_,_,_,ph) <- createProcess_ cmd (proc cmd args)
        { std_in = UseHandle i
        , std_out = UseHandle o
        , std_err = UseHandle e
        , close_fds = True
        }
    pl
    res <- (waitProc cmd args ph `onException` terminateProcess ph) `finally` pw
    case res of
        Nothing -> pure ()
        Just f  -> throwIO f


readPrXX :: MonadIO io => PrXX a -> io String
readPrXX (PrXX f) = liftIO $ do
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

readWritePrXX :: MonadIO io => PrXX a -> String -> io String
readWritePrXX (PrXX f) input = liftIO $ do
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

writePrXX :: MonadIO io => PrXX a -> String -> io a
writePrXX (PrXX f) input = liftIO $ do
    (r,w) <- createPipe
    fst <$> concurrently
        (f r stdout stderr (pure ()) (hClose r))
        (hPutStr w input `finally` hClose w)


instance Applicative Proc where
    pure a = PP $ \_ _ _ -> pure (pure a)
    (PP f) <*> (PP a) = PP $ \i o e -> do
        f' <- f i o e
        pure $ do
            f'' <- f'
            print "Done f'"
            a'' <- join $ a i o e
            print "Done a'"
            pure $ f'' a''
            

-- | The @Semigroup@ instance for @Proc@ pipes the stdout of one process
-- into the stdin of the next. However, consider using `|>` instead which
-- behaves when used in an @IO@ context. If you use `<>` in an IO monad
-- you will be using the `IO` instance of semigroup which is a sequential
-- execution. @|>@ prevents that error.
instance Semigroup (Proc a) where
    a <> b = PP $ \i o e -> do
        (r,w) <- createPipe
        let
            run = do
                fst <$> (runPP a i w e $ do
                    runPP b r o e (pure ())
                    )
        pure $ run `finally` (hClose r `finally` hClose w)


instance Monad Proc where
    a >>= f = PP $ \i o e -> do
        a' <- fst <$> runPP a i o e (pure ())
        let PP f' = f a'
        f' i o e

instance MonadIO Proc where
    liftIO io = PP $ \_ _ _ -> pure io

waitProc :: String -> [String] -> ProcessHandle -> IO (Maybe Failure)
waitProc cmd arg ph = waitForProcess ph >>= \case
    ExitFailure c
        | fromIntegral c == negate sigPIPE -> pure Nothing
        | otherwise -> pure $ Just $ createFailure cmd arg c
    ExitSuccess -> pure Nothing

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Create a @Proc@ from a command and a list of arguments.
mkProc :: String -> [String] -> Proc ()
mkProc cmd args = PP $ \i o e -> do
    (_,_,_,ph) <- createProcess_ cmd (proc cmd args)
        { std_in = UseHandle i
        , std_out = UseHandle o
        , std_err = UseHandle e
        , close_fds = True
        }
    -- TODO:
    -- If an exception is received while waiting, we should terminate the process
    -- somehow. (we could be getting async exceptions, and we need to clean up).
    pure $ do
        onException
            (waitProc cmd args ph)
            (terminateProcess ph)
            >>= \case
                Nothing -> pure ()
                Just f -> throwIO f
    

catchFailure :: IO a -> IO (Either Failure a)
catchFailure = try

catchCode :: IO a -> IO Int
catchCode a = catchFailure a >>= \case
    Right _ -> pure 0
    Left f  -> pure $ code f

-- | Run a @Proc@ in any @MonadIO@ (including other @Proc@s).
runProc :: MonadIO io => Proc a -> io a
runProc p = liftIO $ do
    fst <$> runPP p stdin stdout stderr (pure ())

-- | Read the stdout of a @Proc@ in any @MonadIO@ (including other @Proc@s).
-- This is strict, so the whole output is read into a @String@. See @withRead@
-- for a lazy version that can be used for streaming.
readProc :: MonadIO io => Proc a -> io String
readProc p = liftIO $ do
    bracket
        createPipe
        (\(r,w) -> finally (hClose r) (hClose w))
        $ \(r,w) -> do
            fmap snd $ runPP p stdin w stderr $ do
                hClose w
                output <- hGetContents r
                C.evaluate $ rnf output
                pure output



-- runPP proc postLaunch postWait
runPP :: Proc a -> Handle -> Handle -> Handle -> IO b -> IO (a, b)
runPP (PP f) i o e postLaunch = do
    mask $ \restore -> do
        w <- f i o e
        -- TODO: Is this OK? `w` could block, so does it need to be
        -- restored'd? Do I need to split `w` into two IO actions, one
        -- to wait, and one to kill? I'll rejig everything to use runPP
        -- then I'll look into that.
        r <- restore postLaunch `finally` w
        a <- w
        pure (a,r)



-- | Run a process and capture it's output lazily. Once the continuation
-- is completed, the handles are closed, and the process is terminated.
withRead :: MonadIO io => Proc a -> (String -> IO b) -> io b
withRead p k = do
    liftIO $
        bracket
            createPipe
            (\(r,w) -> finally (hClose r) (hClose w))
            $ \(r,w) -> do
                snd <$> (runPP p stdin w stderr $ do
                    hClose w
                    out <- hGetContents r
                    k out `finally` hClose r
                    )

writeProc :: MonadIO io => Proc a -> String -> io a
writeProc p input = liftIO $ do
    bracket
        createPipe
        (\(r,w) -> hClose r `finally` hClose w)
        (\(r,w) -> fst <$> (runPP p r stdout stderr $ do
            hPutStr w input
            hClose w
            )
        )

readTrim :: MonadIO io => Proc a -> io String
readTrim = fmap trim . readProc

interact :: MonadIO io => Proc a -> (String -> String) -> io a
interact (PP f) p = liftIO $ do
    (ir,iw) <- createPipe
    (or,ow) <- createPipe
    wa <- f ir ow stderr
    hClose ir
    hClose ow
    out <- hGetContents or
    hPutStr iw (p out)
    hClose iw
    hClose or
    wa


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

instance ExecArgs (PrXX ()) where
    toArgs (cmd:args) = mkPrXX cmd args

instance (ExecArg b, ExecArgs a) => ExecArgs (b -> a) where
    toArgs f i = toArgs $ f ++ [asArg i]
-- 
-- instance ExecArgs [String] where
--     toArgs = id

-- | Commands can be executed directly in IO (this goes via the @CmdT@ instance)
instance ExecArgs (IO ()) where
    toArgs = runPrXX . toArgs

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

readCreateProcessInputHandle :: CreateProcess -> StdStream -> IO String
readCreateProcessInputHandle cp input = do
    let 
        cp_opts = cp
            { std_in  = input
            , std_out = CreatePipe
            }
    (ex, output) <- withCreateProcess cp_opts $
      \_ (Just outh) _ ph -> do

        output  <- hGetContents outh
        C.evaluate $ rnf output
        ex <- waitForProcess ph
        return (ex, output)

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> createProcessToFailure cp_opts r

createProcessToFailure :: CreateProcess -> Int -> IO a
createProcessToFailure CreateProcess{cmdspec=s} i =
    case s of
        ShellCommand{} -> error "We don't handle shell commands"
        RawCommand f a -> throw $ Failure f a i

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
