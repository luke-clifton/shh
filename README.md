# Shh

[![](https://img.shields.io/hackage/v/shh.svg?colorB=%23999)](http://hackage.haskell.org/package/shh)
[![](https://travis-ci.org/luke-clifton/shh.svg?branch=master)](https://travis-ci.org/luke-clifton/shh)

Shh is a library to enable convinient shell-like programming in Haskell.
It works well in scripts, and from GHCi, allowing you to use GHCi as a shell.

It is also a wrapper tool around launching GHCi as a shell.

It supports

 * Automatically defining a function for each executable on your `$PATH`
   using template Haskell, as well as a runtime check to ensure they all
   exist on startup.

 * Redirction of stdout and stderr
       
       -- Redirect stdout
       λ echo "Hello" &> StdErr
       λ echo "Hello" &> Truncate ".tmp_file"

       -- Redirect stderr
       λ echo "Hello" &!> Append "/dev/null"
       λ echo "Hello" &!> StdOut


 * Piping stdout or stderr to the input of a chained process
       
       λ cat "/dev/urandom" |> xxd |> head "-n" 5

 * Multiple processes sequentially feeding a single process

       λ (echo 1 >> echo 2) |> cat

 * Use of Haskells concurrency primitives.

       λ race (sleep 1) $ curl "http://this_needs_to_timeout_after_1_second"

       λ d <- readTrim $ mktemp "-d"
       λ :{
       | System.Directory.withCurrentDirectory d $ do
       |   mapConcurrently_ (curl "-LOJs")
       |     [ "https://raw.githubusercontent.com/luke-clifton/shh/master/shell.nix"
       |     , "https://raw.githubusercontent.com/luke-clifton/shh/master/shh.cabal"
       |     ]
       |   ls
       | :}

 * Capturing of process output

       λ loggedIn <- nub . words <$> readProc users
       λ putStrLn $ "Logged in users: " ++ show loggedIn

       λ mapM_ putStrLn =<< readSplit0 (Shh.Example.find "-maxdepth" 1 "-print0")

 * Capturing infinite output of a process lazily

       λ withRead (cat "/dev/urandom" |> xxd) $ mapM_ putStrLn . take 3 . lines
       00000000: 8fcb ebee 9228 a897 3bfc 1d05 491d aceb  .....(..;...I...
       00000010: 47de 3ea3 2788 44ac 9b85 0a0f a458 b949  G.>.'.D......X.I
       00000020: 5308 ddfe 5790 5a5f 39e3 bbb6 b689 2b03  S...W.Z_9.....+.

 * Write strings to stdin of a process.

       λ writeProc cat "Hello\n"
       Hello

       λ "Hello" >>> shasum
       f7ff9e8b7bb2e09b70935a5d785e0cc5d9d0abf0  -

       λ shasum <<< "Hello"
       f7ff9e8b7bb2e09b70935a5d785e0cc5d9d0abf0  -

 * Proper exceptions, when a process exits with a failure code, an exception
   is thrown. You can catch these normally. The exception includes the error
   code, the command, and all it's arguments.

       λ false "Ha, it died"
       *** Exception: Command `false "Ha, it died"` failed [exit 1]

       λ catchCode false
       1

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
might introduce a more succinct globbing feature.

## Usage

Enable Temlpate Haskell and load the environment

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

### Script Usage

TODO: Fill this in once the user experience is better.
    
