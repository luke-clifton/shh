{-# LANGUAGE TemplateHaskell #-}

module Shh.Example where

import Shh

$(loadAnnotatedEnv (\s -> s ++ "_"))
