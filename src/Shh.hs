{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
-- | Shh provides a shell-like environment for Haskell.
module Shh
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
    -- === Lazy/Streaming reads
    -- These reads are lazy. The process is run long enough to produce
    -- the amount of output that is actually used. It is therefor suitable
    -- for use with infinite output streams. The process is terminated
    -- as soon the function finishes. Note that the result is forced to
    -- normal form to prevent any accidental reading after the process has
    -- terminated.
    , withRead
    , withReadSplit0
    , withReadLines
    , withReadWords
    -- | === Strict reads
    , readProc
    , readTrim
    , readSplit0
    , readLines
    , readWords
    , readAuto
    -- | === String manipulation
    -- Utility functions for dealing with common string issues in shell
    -- scripting.
    , trim
    , split0
    -- | == Writing to @stdin@
    , writeProc
    , (<<<), (>>>)
    , readWriteProc
    , apply
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
    , loadAnnotatedEnv
    , loadExe
    , loadExeAs
    ) where

import Shh.Internal
