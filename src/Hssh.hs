{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Hssh where

import System.Exit
import System.IO
import System.Process
import Control.Monad.Writer
import Data.List
import Data.List.Split
import Data.Char
import Control.Monad
import System.Directory
import Language.Haskell.TH
import System.Environment
import Control.Exception as C
import Control.Concurrent.MVar
import Control.Concurrent
import Control.DeepSeq (rnf)

data Failure = Failure
    { prog :: String
    , args :: [String]
    , code :: Int
    }

instance Show Failure where
    show f = intercalate " " $
        [ "callProcess:"
        , prog f
        ]
        ++ map show (args f)
        ++
        [ "(exit"
        , show (code f)
        , "): failed"
        ]

instance Exception Failure

class ExecArgs a where
    toArgs :: [String] -> a

class ExecArg a where
    asArgs :: a -> String

instance ExecArgs a => ExecArgs (String -> a) where
    toArgs f i = toArgs $ f ++ [i]

instance ExecArgs [String] where
    toArgs = id

instance ExecArgs (IO ()) where
    toArgs (cmd:args) =  callProcess cmd args

instance ExecArgs (ReadCmd ()) where
    toArgs (cmd:args) =  ReadCmd $ lift (readProcess cmd args "") >>= tell

newtype ReadCmd a = ReadCmd { runReadCmd :: WriterT String IO a }
    deriving (Functor, Applicative, Monad)

readCmd :: ReadCmd () -> IO String
readCmd = execWriterT . runReadCmd

newtype PipeCmd  = PipeCmd { unPipe :: [CreateProcess] }
    deriving Monoid

runPipe' :: (CreateProcess -> StdStream -> IO a) -> PipeCmd -> IO a
runPipe' handler (PipeCmd procs) = go Inherit procs
    where
        -- go :: StdStream -> StdStream -> [CreateProcess] -> IO a
        go inp [last] = handler last inp
            -- withCreateProcess (last {std_in = inp, std_out = out}) $ \_ sout _ ph -> handler ph sout
        go inp (next:rest@(_:_)) = do
            withCreateProcess (next {std_in = inp, std_out = CreatePipe}) $ \_ (Just sout) _ ph -> do
                r <- go (UseHandle sout) rest
                e <- waitForProcess ph
                case e of
                    ExitSuccess -> return r
                    ExitFailure r -> createProcessToFailure next r

runPipe :: PipeCmd -> IO ()
runPipe = runPipe' $ \cp h -> withCreateProcess cp{std_in = h} $ \_ _ _ ph -> do
    e <- waitForProcess ph
    case e of
        ExitSuccess -> return ()
        ExitFailure r -> createProcessToFailure cp r

readPipe :: PipeCmd -> IO String
readPipe = runPipe' readCreateProcessInputHandle

instance ExecArgs PipeCmd where
    toArgs (cmd:args) = PipeCmd [proc cmd args]

class Unit a
instance {-# OVERLAPPING #-} Unit b => Unit (a -> b)
instance {-# OVERLAPPABLE #-} a ~ () => Unit (m a)
instance {-# OVERLAPPABLE #-} Unit PipeCmd

pathBins :: IO [FilePath]
pathBins = do
    paths <- splitOn ":" <$> getEnv "PATH"
    paths <- filterM doesDirectoryExist paths
    bins <- nub . concat <$> mapM getDirectoryContents paths
    return $ flip filter bins $ \p -> all isLower p && not (p `elem` ["import", "if", "else", "then", "do", "in", "let", "type"])

loadPath :: String -> Q [Dec]
loadPath path =
    let
        impl = valD (varP (mkName path)) (normalB [|
            toArgs [] path 
            |]) []
        name = mkName path
        typn = mkName "a"
        typ = SigD (mkName path) (ForallT [PlainTV typn] [AppT (ConT ''Unit) (VarT typn), AppT (ConT ''ExecArgs) (VarT typn)] (VarT typn))
    in do
        i <- impl
        return $ [typ,i]

loadEnv :: Q [Dec]
loadEnv = do
    bins <- runIO pathBins
    fmap join $ mapM loadPath bins


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
