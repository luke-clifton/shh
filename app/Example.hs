{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Control.Monad
import Hssh
import Data.Monoid
import Data.Char
import Data.List

$(loadEnv)

main :: IO ()
main = (sleep 1 >> echo "Hello" >> sleep 2) |> cat
