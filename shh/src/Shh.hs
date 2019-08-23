{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
-- | Shh provides a shell-like environment for Haskell.
module Shh
    ( initInteractive
    -- | == Running a `Proc`
    , Shell(..)
    -- | == Constructing a `Proc`
    -- You usually don't need to @`runProc`@ because most functions in shh
    -- are polymorphic in their return type.
    -- | === External Processes
    -- These allow for the construction of @`Proc`@s that call external
    -- processes. You will often use the TemplateHaskell functions below
    -- to create these.
    , exe
    , mkProc
    , mkProc'
    , Proc()
    -- | === "Native" Processes (Lazy)
    -- You can also create native Haskell @`Proc`@s which behave the same
    -- way, but simply run Haskell functions instead of external processes.
    -- 
    -- NB: The functions here that operate on lazy @`ByteString`@s read from
    -- @stdin@, and can be used in a streaming fashion.
    -- These reads are lazy. The process is run long enough to produce
    -- the amount of output that is actually used. It is therefor suitable
    -- for use with infinite output streams. The feeding process has it's
    -- @stdout@ closed as soon the function finishes. Note that the result
    -- is forced to normal form to prevent any accidental reading after
    -- the process has terminated.
    , pureProc
    , writeOutput, writeError
    , prefixLines
    , readInput
    , readInputEndBy
    , readInputEndBy0
    , readInputLines
    , readInputP
    , readInputEndByP
    , readInputEndBy0P
    , readInputLinesP
    , xargs1
    -- | === Extracting output to Haskell (Strict)
    -- These functions are trivially implemented in terms of the above. Note
    -- that they are strict.
    , capture
    , captureTrim
    , captureEndBy
    , captureEndBy0
    , captureLines
    -- | == Piping and Redirection
    , (|>)
    , (|!>)
    , pipe
    , pipeErr
    , (&>)
    , (&!>)
    , (<|)
    , Stream(..)
    , devNull
    -- | === Writing to @stdin@
    -- NB: See also `writeOutput` for an `echo`-like option. These are all
    -- implemented in terms of `writeOutput`.
    , (<<<), (>>>), writeProc
    , apply
    -- | === String manipulation
    -- Utility functions for dealing with common string issues in shell
    -- scripting.
    , trim
    , endBy
    , endBy0
    -- | == Exceptions
    -- If any exception is allowed to propagate out of a pipeline, all the
    -- processes comprising the pipeline will be terminated. This is contrary
    -- to how a shell normally works (even with @-o pipefail@!).
    , Failure(..)
    , ignoreFailure
    , tryFailure
    , failWithStdErr
    , exitCode
    -- | == Constructing Arguments
    , Cmd
    , ExecArg(..)
    , Command()
    , displayCommand
    -- | == Template Haskell helpers
    , encodeIdentifier
    , ExecReference(..)
    , load
    , loadEnv
    , loadFromDirs
    , loadFromBins
    , loadAnnotated
    , loadAnnotatedEnv
    , loadAnnotatedFromDirs
    , loadExe
    , loadExeAs
    , pathBins
    , pathBinsAbs
    -- | = Builtins
    , cd
    ) where

import Shh.Internal
