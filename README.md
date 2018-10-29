# Hssh

A template Haskell hack to create Haskell functions from every executable on
your `$PATH` environment variable. Makes using GHCi as a shell really quite
nice.

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
    
