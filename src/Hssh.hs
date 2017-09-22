{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module Hssh where

import System.Process
import Control.Monad.Writer
import Data.List
import Data.List.Split
import Data.Char
import Control.Monad
import System.Directory
import Language.Haskell.TH
import System.Environment

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

newtype PipeCmd = PipeCmd { unPipe :: [CreateProcess] }
    deriving Monoid

runPipe :: PipeCmd -> IO ()
runPipe (PipeCmd procs) = go Inherit procs
    where
        go :: StdStream -> [CreateProcess] -> IO ()
        go inp [last] = withCreateProcess (last {std_in = inp}) $ \_ _ _ ph -> void $ waitForProcess ph
        go inp (next:rest@(_:_)) = do
            withCreateProcess (next {std_in = inp, std_out = CreatePipe}) $ \sin (Just sout) serr ph -> do
                go (UseHandle sout) rest
                e <- waitForProcess ph
                return ()

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
    return $ flip filter bins $ \p -> all isLower p && not (p `elem` ["import", "if", "else", "then", "do", "in", "let"])

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
