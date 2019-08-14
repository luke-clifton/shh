# Shh

[![](https://img.shields.io/hackage/v/shh.svg?colorB=%23999&label=shh)](http://hackage.haskell.org/package/shh)
[![](https://img.shields.io/hackage/v/shh-extras.svg?colorB=%23999&label=shh-extras)](http://hackage.haskell.org/package/shh-extras)
[![](https://builds.sr.ht/~lukec/shh/nix.yml.svg)](https://builds.sr.ht/~lukec/shh/nix.yml?)

<details><summary>
Shh is a library to enable convinient shell-like programming in Haskell.
It works well in scripts, and from GHCi, allowing you to use GHCi as a shell.
</summary>

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Readme (test) where

import Shh

import Control.Concurrent.Async
import Prelude hiding (head)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified System.Directory
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.List (nub)
import Data.Char

load SearchPath ["echo", "base64", "cat", "head", "sleep", "mktemp", "ls", "wc", "find", "tr", "users", "sha256sum", "false", "true"]

curl :: Cmd
curl = true

test :: IO ()
test = do
```

</details>

It's primary purpose is in replacing shell scripts. As such, many
functions are provided to mimic the shell environment, and porting shell
scripts to shh should be fairly straightforward. A simple
["cargo culting" port](docs/porting.md) should work in most situations,
and perhaps be even more robust than the original.

It is also a wrapper tool around launching GHCi as a shell.

It supports

 * Automatically defining a function for each executable on your `$PATH`
   using template Haskell, as well as a runtime check to ensure they all
   exist on startup.

 * Redirction of stdout and stderr
       
   ```haskell
     -- Redirect stdout
     echo "Hello" &> StdErr
     echo "Hello" &> Truncate ".tmp_file"
   
     -- Redirect stderr
     echo "Hello" &!> Append "/dev/null"
     echo "Hello" &!> StdOut
   ```


 * Piping stdout or stderr to the input of a chained process
   
   ```haskell
     cat "/dev/urandom" |> base64 |> head "-n" 5
   ```

 * Multiple processes sequentially feeding a single process

   ```haskell
     (echo 1 >> echo 2) |> cat
   ```

 * Use of Haskells concurrency primitives.

   ```haskell
     race (sleep 1 >> echo "Slept for 1") (sleep 2 >> echo "Slept for 2")

   ```

   ```haskell
     mapConcurrently_ (\url -> curl "-Ls" url |> wc)
       [ "https://raw.githubusercontent.com/luke-clifton/shh/master/shell.nix"
       , "https://raw.githubusercontent.com/luke-clifton/shh/master/README.md"
       ]
   ```

 * Capturing of process output

   ```haskell
     s <- echo "Hello" |> tr "-d" "l" |> capture
     print s

     loggedIn <- nub . Char8.words <$> (users |> capture)
     putStrLn $ "Logged in users: " ++ show loggedIn

     mapM_ Char8.putStrLn =<< (find "-maxdepth" 1 "-print0" |> captureEndBy0)
   ```

 * Capturing infinite output of a process lazily

   ```haskell
     cat "/dev/urandom"
       |> base64
       |> readInput (mapM_ Char8.putStrLn . take 3 . Char8.lines)
   ```

 * Write strings to stdin of a process.

   ```haskell
     writeOutput "Hello\n" |> cat
     -- Hello

     "Hello" >>> sha256sum

     sha256sum <<< "Hello"
   ```

 * Proper exceptions, when a process exits with a failure code, an exception
   is thrown. You can catch these normally. The exception includes the error
   code, the command, and all it's arguments.

   ```haskell
     false "Ha, it died"
     --  *** Exception: Command `false "Ha, it died"` failed [exit 1]
   ```
   ```haskell
     exitCode false
     --  1
   ```

 * "Native" processes, i.e. Haskell functions that behave like a process.

   ```haskell
     echo "Hello" |> pureProc (Char8.map toUpper) |> tr "-d" "L"
     -- HEO
   ```

 * And much, much more! Look at the documentation on Hackage for a
   comprehensive overview of all the possibilities.

## Mnemonics 

Shh has many symbols that might seem intimidating at first, but there
is a simple mnemonic for them.

    |     Piping. Looks like a pipe, same as in POSIX shells.
    &     Redirection, think of the shell `2>&1`
    >,<   The direction of flow of a command
    !     Operate on stderr instead of stdout

So, for example,

    ls |> cat      Pipe the stdout of `ls` into stdin of `cat`
    cat <| ls      Same as above
    ls &> StdErr   Redirect stdout of `ls` to wherever stderr is going.
    StdErr <& ls   Same as above
    ls &!> StdOut  Redirect stderr of `ls` to wherever stdout is going.
    StdOut <!& ls  Same as above

## Globbing

Currently Shh does not have any built in globbing support. Rather, it is
currently suggested to use another library to do globbing. For example,
using the [Glob](http://hackage.haskell.org/package/Glob) package, it is
possible to do something like

    wc =<< glob "*.md"

Certainly more verbose than the Bash equivalent, however, also more explicit,
which is probably a good thing. If this turns out to be too cumbersome, we
might introduce a more succinct globbing feature, though it will always be
explicit, and thus always more verbose than most other shells.

## Usage

Enable Template Haskell and load the environment

    {-# LANGUAGE TemplateHaskell #-}
    $(loadEnv SearchPath)

You now have all your executables available as simple to read
Haskell functions.

If you want to check that all the dependenies still exist, you can use
`missingExecutables :: IO [String]`, which will tell you if anything is
missing.

### Usage in GHCi

If you want `^D` to be recognised as a EOF marker (when running commands
that read from stdin) when running in GHCi, you will need to run the
`initInteractive` function. This sets the line buffering appropriately and
ensures the terminal is in canonical mode.

### Shh as a Shell

There is a tool called `shh` which is a fairly small wrapper around launching
GHCi which automatically loads your environment and allows you to have custom
config when using GHCi as a shell.

To install it, one option is to use `cabal new-install`

```bash
cabal new-install --lib shh
cabal new-install --lib ssh-extras
```

The `shh` binary will look in your `$SHH_DIR` (defaults to `$HOME/.shh`) for
a `Shell.hs`, `init.ghci` and `wrapper` files. If these don't exist default
ones will be created.

The `Shell.hs` file should contain any top level definitions that you would
like to be available in your Shell. By default it loads your environment.

The `init.ghci` file is loaded by GHCi after your `.ghci` files. This lets
you specify settings that you want to take effect when using GHCi as a shell.
By default it sets a shell-like prompt.

The `wrapper` file is an executable that is called with the command that is
to be executed. By default it just calls `exec` with the arguments passed to
it. The use-case for this is to be able to set up the environment for `shh`.
You might, for example, wrap the execution in a `nix-shell`. Either way,
it is up to you to make sure that the compiler, and packages you require are
available, either globally, or provided by the `wrapper` script.

#### Faster Startup

`shh` precompiles your `Shell.hs` file so that starting up `shh` is very
quick on subsequent launches. Unfortunately, `shh` isn't quite able to detect
this perfectly. If you see GHCi telling you that it is `Compiling Shell`,
and you notice the delay when starting `shh`, try manually forcing a rebuild
by passing in the `--rebuild` argument to `shh`.

This is particularly likely to happen if you upgrade your GHC, or installed
packages, or even `shh` itself.

#### Nix Wrapper Example

The following snippet could act as a `wrapper` file to set up a suitable
environment using `nix-shell`

    #! /usr/bin/env nix-shell
    #! nix-shell -i bash -p "(haskellPackages.ghcWithPackages (p: with p; [shh shh-extras]))"
    exec "$@"

### Script Usage

#### Nix

Nixpkgs provides a `writeHaskellBin` function which is very convenient for
writing quick scripts for your Nix setup.

```nix
writers.writeHaskellBin "example" {libraries = [haskellPackages.shh];} ''
  {-# LANGUAGE TemplateHaskell #-}
  import Shh

  -- Load binaries from Nix packages. The dependencies will be captured
  -- in the closure.
  loadFromBins ["${git}", "${coreutils}", "${curl}"]

  main :: IO ()
  main = do
    cat "/a/file"
    cp "/a/file" "/b/file"
''
```

## Alternatives

There are quite a few players in the "shell programming for Haskell" field.

This table attempts to summarise some of the differences.

 * `Pipe Style` refers to how processes are joined together, "native" means
   that the mechanisms provided by the OS are used, while "via Haskell" means
   that the data is read into the Haskell process, and then written into the
   subprocess.
 * `Via Shell` refers to whether subprocesses are launched directly or via
   a shell (which can provide a "native" piping solution at the cost of
   composability)
 * `Run in IO` refers to whether commands need to be prefixed with `run` or
   similar functions to actually execute them.
 * `TH Helper` refers to whether the use of TH to generate Haskell functions
   based on commands found at compile time is encouraged in the main library.
 * `Monadic Subshell` refers to the ability to join multiple processes together
   and feed them all from the same input and to the same output.
   `echo a | (cat; echo b) | wc -l` should report that 2 lines appeared.


| Library | Pipe Style  | Via Shell | Run in IO | Threadsafe `cd` | TH Helper | Monadic Subshell | Redirect `stderr` |
|---------|-------------|-----------|-----------|-----------------|-----------|------------------|-------------------|
| Shh     | Native      | No        | Yes       | No              | Yes       | Yes              | Yes               |
| Shelly  | Via Haskell | Yes       | No        | Yes             | No        | No               | Yes               |
| Turtle  | Via Haskell | Optional  | No        | ?               | No        | No (Alternative) | Yes               |
| shell-conduit | Via Haskell | Optional | No   | ?               | Yes       | Yes              | No?               |


### Errors

| Library | Exception on non-zero | Contains arguments | Contains `stderr` | Terminates pipeline |
|---------|-----------------------|--------------------|-------------------|---------------------|
| Shh     | Yes                   | Yes                | Optional          | Yes                 |
| Shelly  | Yes                   | Yes                | Yes               | Yes                 |
| Turtle  | Sometimes             | No                 | No                | ?                   |
| shell-conduit | Yes             | Yes                | No                | No                  |

