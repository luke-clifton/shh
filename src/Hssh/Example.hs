{-# LANGUAGE TemplateHaskell #-}

module Hssh.Example where

import Hssh

$(loadAnnotatedEnv (\s -> s ++ "_"))
