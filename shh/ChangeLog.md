# Revision history for shh

## 0.7.0.0 -- 2019-08-06

This is a fairly major refactor which consolidates a bunch of type classes
and simplifies a few things.

* ExecArgs, Unit, PipeResult, PipeFailure are all gone and replaced
  with Command and Shell type classes.
* Renamed various functions.
  * catchFailure -> tryFailure
  * catchCode    -> exitCode
* Remove some unnecessary utf8 decoding.


## 0.6.0.0 -- 2019-06-26

This change doesn't remove any functions or majorly change any semantics,
but it will break everything. We now use ByteString instead of String as
the basis for interaction with the OS. This has the potential to improve
performance, but most importantly, helps with correctness.

* Switch to a ByteString interface, in the process fixing up a bunch of
  unicode issues.
* Encode String type arguments as utf8.

## 0.5.0.0 -- 2019-05-23

* Change how identifiers are encoded to avoid clashes in all scenarios
  (Potentially breaking change)

## 0.4.0.0 -- 2019-04-20

* Pre-compile Shell.hs for faster loading of shh shell
* Introduce `nativeProc` interface and related functions (`pureProc`...)
* Allow type-changing `|>` which enables `capture` and
  similar "processes" to replace the less consistent `readProc`
  family of functions.  `s <- readProc $ echo "Hello"` can now
  be written `s <- echo "Hello" |> capture`. This allows capturing
  within the `Proc` monad to manipulate the stream in Haskell.
* Introduce `xargs1` function which can replace some uses of the `xargs`
  utility, and provides a type-checked, and spell-checked interface
  similar to `xargs`.

## 0.3.X.X -- 2019-03-10

* Changes how lazy reading works. We no longer terminate the process, we
  just close the handles and wait for the process to terminate naturally.

  This eliminates a source of non-determinism

## 0.2.X.X -- 2019-01-23

* Extended the ExecArg typeclass to handle lists.

## 0.1.X.X  -- 2018-11-02

* First version. Released on an unsuspecting world.
