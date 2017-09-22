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
main = do
    -- Your executables are available as functions
    ls

    -- Pass in arguments
    ls "-a"

    -- Pipe things to different processes
    runPipe $ ls <> wc "-c"

    -- Read the result of a command
    print =<< length <$> readCmd ls

    -- Or read the result of a pipe
    line <- readPipe (env <> grep "^USER=") >>= print . map toUpper

    -- Read the result of a series of commands.
    r <- readCmd $ do
        u <- nub . words <$> readCmd users
        forM_ u $ \user -> do
            finger user
    putStrLn r
