# Hssh

Hssh is a library to enable convinient shell-like programming in Haskell.
It works well in scripts, and from GHCi, allowing you to use GHCi as a shell.

It supports

 * Redirction of stdout and stderr
       
    λ echo "Hello" &> StdErr
    λ echo "Hello" &> Truncate ".tmp_file"
    λ echo "Hello" &!> Append "/dev/null"

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
    |     [ "https://raw.githubusercontent.com/luke-clifton/hssh/master/shell.nix"
    |     , "https://raw.githubusercontent.com/luke-clifton/hssh/master/hssh.cabal"
    |     ]
    |   ls
    | :}
    hssh.cabal  shell.nix


## Mnemonics 

Hssh has many symbols that might seem intimidating at first, but there
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

## Piping

Supports shell like piping.

    cat "/dev/urandom" |> xxd |> head

## Output capture

Supports capturing the output of commands as a `String`

    loggedInUsers <- nub . words <$> readProc users
    putStrLn loggedInUsers

### Lazy capture

Read stdout in lazily.

    withRead (cat "/dev/urandom" |> xxd) $ \ouput -> do
    	mapM_ putStrLn $ take 10 $ lines $ output

## Redirection

    ls &> Append "result.txt"

## Usage

Enable Temlpate Haskell and load the environment

    {-# LANGUAGE TemplateHaskell #-}
    $(loadEnv)

You now have all your executables available as simple to read
Haskell functions.

### Script Usage

TODO: Fill this in once on Hackage.
    
## Usage in GHCi

If you want `^D` to be recognised as a EOF marker (when running commands
that read from stdin) when running in GHCi, you will need to run the
`initInteractive` function. This sets the line buffering appropriatly and
ensures the terminal is in canonical mode.
