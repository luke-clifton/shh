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
    -- | === "Native" Processes
    -- You can also create native Haskell @`Proc`@s which behave the same
    -- way, but simply run Haskell functions instead of external processes.
    -- 
    -- NB: The functions here that operate on @String@s from @stdin@ read them
    -- lazily, and can be used in a streaming fashion.
    , pureProc
    , writeOutput, writeError
    , prefixLines
    , capture
    , captureTrim
    , captureSplit
    , captureSplit0
    , captureLines
    , readInput
    , readInputSplit
    , readInputSplit0
    , readInputLines
    , readInputP
    , readInputSplitP
    , readInputSplit0P
    , readInputLinesP
    , xargs1
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
    --
    -- NB: See `readInput` and `pureProc` for more flexible options to those
    -- listed here.
    , withRead
    , withReadSplit0
    , withReadLines
    , withReadWords
    -- | === Strict reads
    -- NB: See also `capture`
    , readProc
    , readTrim
    , readSplit0
    , readLines
    , readWords
    , readAuto
    -- | === Writing to @stdin@
    -- NB: See also `writeOutput` for an `echo`-like option.
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
