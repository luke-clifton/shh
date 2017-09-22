{-# LANGUAGE TemplateHaskell #-}

module Hssh.Example where

import Control.Monad
import Hssh
import Hssh.TH
import System.Directory
import Data.Monoid

$(loadEnv)

example :: IO ()
example = do
    ls
    runPipe $ ls <> wc "-c"
    print =<< length <$> readCmd ls
    runPipe $ env <> grep "^EDITOR"
