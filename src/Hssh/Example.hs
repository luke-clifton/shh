{-# LANGUAGE TemplateHaskell #-}

module Hssh.Example where

import Control.Monad
import Hssh
import System.Directory
import Data.Monoid
import Data.Char
import Data.List

$(loadEnv)

main :: IO ()
main = cat
--      -- Your executables are available as functions
--      ls
--  
--      -- Pass in arguments
--      ls "-a"
--  
--      -- Pipe things to different processes
--      ls |> wc "-c"
--  
--      -- Read the result of a command
--      print =<< length <$> readProc ls
--  
--      -- Or read the result of a pipe
--      readProc (env |> grep "^USER=") >>= putStr . map toUpper
--  
--      -- Read the result of a series of commands.
--      r <- readProc $ do
--          u <- nub . words <$> readProc users
--          forM_ u $ \user -> do
--              finger user
--  
--      putStrLn (map toUpper r)
