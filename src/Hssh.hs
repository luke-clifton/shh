{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Hssh where

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

-- | When a process exits with a non-zero exit code
-- we throw this Failure exception.
--
-- The only exception to this is when a process is terminated
-- by SIGPIPE in a pipeline, in which case we ignore it.
data Failure = Failure
    { prog :: String
    , args :: [String]
    , code :: Int
    }

instance Show Failure where
    show f = intercalate " " $
        [ "Command"
        , show $ prog f
        ]
        ++ map show (args f)
        ++
        [ "failed [exit"
        , show (code f) ++ "]"
        ]

instance Exception Failure

-- | The type of a pipeline of commands (or even just a single one, it's
-- just a short pipe)
--
-- Create this type using the @ExecArgs@ instance.
newtype Pipe = P [CreateProcess]
    deriving (Monoid)


-- | A monad transformer for sequencing (as oposed to piping) commands.
-- Create this type using the @ExecArgs@ instance.
newtype CmdT m a = CmdT {unCmdT :: FreeT (Coyoneda (U Pipe)) m a}
    deriving (Functor, Applicative, Monad, MonadIO)

-- | A command monad specialised to IO.
type Cmd = CmdT IO

-- | Run a command in any monad that stack that can do IO
runCmd :: forall m a. MonadIO m => CmdT m a -> m a
runCmd (CmdT u) = do
    a <- runFreeT u
    interpret a

    where
    interpret :: forall b. FreeF (Coyoneda (U Pipe)) b (FreeT (Coyoneda (U Pipe)) m b) -> m b
    interpret (Pure a) = pure a
    interpret (Free (Coyoneda f (U p))) = do
        r <- runPipe p
        i <- runFreeT $ f r
        interpret i

-- | `runCmd` specialised to IO
runCmd' :: Cmd a -> IO a
runCmd' = runCmd

-- | Read the result of a sequence of commands into a @String@
readCmd :: forall m a. MonadIO m => CmdT m a -> m String
readCmd (CmdT u) = do
    a <- runFreeT u
    execWriterT $ interpret a
    where
        interpret :: forall b. FreeF (Coyoneda (U Pipe)) b (FreeT (Coyoneda (U Pipe)) m b) -> WriterT String m b
        interpret (Pure a) = pure a
        interpret (Free (Coyoneda f (U p))) = do
            r <- liftIO $ readPipe p
            tell r
            i <- lift $ runFreeT $ f ()
            interpret i


-- | Helper function for building up @runPipe@ and @readPipe@.
runPipe' :: MonadIO io => (CreateProcess -> StdStream -> IO a) -> Pipe -> io a
runPipe' handler (P procs) = liftIO $ go Inherit procs
    where
        go inp [last] = handler last inp
        go inp (next:rest@(_:_)) = do
            withCreateProcess (next {std_in = inp, std_out = CreatePipe}) $ \_ (Just sout) _ ph -> do
                r <- go (UseHandle sout) rest
                -- TODO: Should we async wait for process so that early failures
                -- in a pipeline will kill upstream programs that can continue?
                --
                --     runPipe $ false <> less
                --
                -- does not terminate immediately in it's current form, though
                -- the exception is still thrown after less completes.
                e <- waitForProcess ph
                case e of
                    ExitFailure code
                        -- Within a pipeline, intermediate results can fail with SIGPIPE.
                        -- In bash they can fail with anything.
                        | fromIntegral code == negate sigPIPE -> return ()
                        | otherwise -> createProcessToFailure next code
                    ExitSuccess -> return ()
                return r

-- | Run a pipeline. The @Monoid@ instance of @Pipe@ creates a pipe between
-- the processes, feeding the output of one into the input of another. Very
-- similar to Bash pipelines, except we throw execptions when a process exits
-- with a failure condition other than SIGPIPE.
--
-- >>> runPipe $ cat "/dev/urandom" <> xxd <> head "-n" 50
runPipe :: MonadIO io => Pipe -> io ()
runPipe = runPipe' $ \cp h -> withCreateProcess cp{std_in = h} $ \_ _ _ ph -> do
    e <- waitForProcess ph
    case e of
        ExitSuccess -> return ()
        ExitFailure r -> createProcessToFailure cp r

-- | Read the result of running a pipeline. See @runPipe@
readPipe :: MonadIO io => Pipe -> io String
readPipe = runPipe' readCreateProcessInputHandle

-- | A class for things that can be converted to arguments on the command
-- line.
class ExecArg a where
    asArg :: a -> String

instance ExecArg String where
    asArg = id

instance ExecArg Int where
    asArg = show

instance ExecArg Integer where
    asArg = show

-- | A class for building up a command
class ExecArgs a where
    toArgs :: [String] -> a

instance (ExecArg b, ExecArgs a) => ExecArgs (b -> a) where
    toArgs f i = toArgs $ f ++ [asArg i]

instance ExecArgs [String] where
    toArgs = id

-- | Commands can be built into a pipe directly.
instance ExecArgs Pipe where
    toArgs (cmd:args) = P [proc cmd args]

-- | Allows you to sequence the @Pipe@ instances.
instance Monad m => ExecArgs (CmdT m ()) where
    toArgs args = CmdT $ liftF $ liftCoyoneda $ U $ toArgs args


-- | Commands can be executed directly in IO (this goes via the @CmdT@ instance)
instance ExecArgs (IO ()) where
    toArgs = runCmd . toArgs

-- | Force a `()` result.
class Unit a
instance {-# OVERLAPPING #-} Unit b => Unit (a -> b)
instance {-# OVERLAPPABLE #-} a ~ () => Unit (m a)
instance {-# OVERLAPPABLE #-} Unit Pipe

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

-- TODO: Does this exist anywhere?
-- | Helper type for building a monad.
data U f a where
    U :: f -> U f ()

