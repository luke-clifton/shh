# Hssh

A template Haskell hack to create Haskell functions from every executable on
your `$PATH` environment variable. Makes using GHCi as a shell really quite
nice.

## Piping

Supports shell like piping.

    cat "/dev/urandom" |> xxd |> head

## Output capture

Supports capturing the output of commands as a `String`

    loggedInUsers <- nub . words <$> readProc users
    putStrLn loggedInUsers

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
    
