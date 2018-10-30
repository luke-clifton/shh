{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Hssh
import Data.Monoid
import Data.Char
import Data.List

$(loadEnv)

main :: IO ()
main = cat
