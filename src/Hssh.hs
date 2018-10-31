{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
-- | Hssh provides a shell-like environment for Haskell.
module Hssh
    ( initInteractive
    -- | == Constructing a `Proc`
    -- You will rarely have to use these as most of the time these are
    -- created for you by using the `loadEnv` template Haskell function.
    , exe
    , mkProc
    , runProc
    , Proc()
    -- | == Piping and Redirection
    , PipeResult(..)
    , (<|)
    , Stream(..)
    , devNull
    -- | == Reading @stdout@
    , readProc
    , withRead
    , trim
    , readTrim
    , split0
    , readSplit0
    , readLines
    , readAuto
    -- | == Writing to @stdin@
    , writeProc
    , (<<<), (>>>)
    , readWriteProc
    , mapP
    -- | == Exceptions
    -- If any exception is allowed to propagate out of a pipeline, all the
    -- processes comprising the pipeline will be terminated. This is contrary
    -- to how a shell normally works (even with @-o pipefail@!).
    , Failure(..)
    , ignoreFailure
    , catchFailure
    , catchCode
    -- | == Constructing Arguments
    , ExecArg(..)
    -- | == Template Haskell helpers
    , loadEnv
    , loadExe
    ) where

import Hssh.Internal
