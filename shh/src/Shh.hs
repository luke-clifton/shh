{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- | Shh provides a shell-like environment for Haskell.
module Shh
    ( initInteractive
    -- | == Constructing a `Proc`
    -- | === External Processes
    -- These allow for the construction of @`Proc`@s that call external
    -- processes. You will often use the TemplateHaskell functions below
    -- to create these.
    , exe
    , mkProc
    , mkProc'
    , runProc
    , Proc()
    -- | === "Internal" Processes
    -- You can also create native Haskell @`Proc`@s which behave the same
    -- way, but simple run Haskell functions instead of external processes.
    , pureProc
    , writeOutput, writeError
    , prefixLines
    , capture
    , readInput
    -- | == Piping and Redirection
    , PipeResult(..)
    , (<|)
    , Stream(..)
    , devNull
    -- | === Lazy/Streaming reads
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
    -- | === Writing to @stdin@
    , (<<<), (>>>), writeProc
    , readWriteProc
    , apply
    -- | === String manipulation
    -- Utility functions for dealing with common string issues in shell
    -- scripting.
    , trim
    , split0
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
    , ExecArgs()
    , Unit()
    -- | == Template Haskell helpers
    , encodeIdentifier
    , ExecReference(..)
    , load
    , loadEnv
    , loadAnnotated
    , loadAnnotatedEnv
    , loadExe
    , loadExeAs
    , pathBins
    , pathBinsAbs
    -- | = Builtins
    , cd
    ) where

import Shh.Internal
