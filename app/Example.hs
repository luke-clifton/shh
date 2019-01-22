{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Control.Monad
import Shh
import Data.Monoid
import Data.Char
import Data.List

$(loadEnv)

-- We could also have been a little more explicit about it.
-- load ["sleep", "echo", "cat"]

main :: IO ()
main = do
    -- Crash the program if we are missing any executables.
    [] <- missingExecutables
    (sleep 1 >> echo "Hello" >> sleep 2) |> cat
