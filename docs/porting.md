# Porting Shell Scripts

This document provides a cheat sheet for porting shell scripts to shh.

## Capturing Output

```bash
a=$(git rev-parse HEAD)
```

```haskell
a <- readTrim $ git "rev-parse" "HEAD"
```

## Piping

```bash
cat sample.txt | grep example
```

```haskell
cat "sample.txt" |> grep "example"
```

## Suppressing stdout

```bash
my_command >/dev/null
```

```haskell
my_command &> devNull
```

## Suppressing stderr

```bash
my_command 2>/dev/null
```

```haskell
my_command &!> devNull
```

## Suppressing both stderr and stdout

```bash
my_command > /dev/null 2>/dev/null
```

```haskell
my_command &> devNull &!> devNull
```

## Getting the exit code of a process

```bash
my_command
exitCode=$?
```

```haskell
exitCode <- catchCode my_command
```

NB: If a command fails in shh, an exception is thrown. `catchCode` suppresses
that exception.

## Subshells

### Sequencing commands in a pipe

```bash
(echo a; echo b) | wc -l
```

```haskell
-- Use the monad instance
(echo "a" >> echo "b") |> wc "-l"
```

NB: You can of course use do notation, and pull it out for clarity.

```haskell
let
    subCommand = do
        echo "a"
        echo "b"
subCommand |> wc "-l"
```

## Exit Traps

### Cleanup

```bash
cleanup() {
    echo "Do cleanup"
}
trap cleanup EXIT
rest_of_prog
```

```haskell
-- Use regular Haskell exception mechanisms.
let cleanup = echo "Do cleanup"
rest_of_prog `finally` cleanup
```

NB: Exceptions can nest arbitrarily, and multiple cleanup actions can be
registered. This get's tricky, and fragile, in bash.

NB: Use bracket for resource acquisition and release.

## Functions

```bash
myFunction() {
    echo a
    echo b
}
```

```haskell
myFunction = do
    echo a
    echo b
```

## Globbing

```bash
wc *.txt
```

```haskell
-- Use the Glob library
import System.FilePath.Glob
wc =<< glob "*.txt"
```

NB: In shh, globbing is always an explicit action, and not something
that can happen accidentally.

## Loops

```bash
for i in $(seq 1 10)
do
    echo "iteration"
    echo "$i"
done
```

```haskell
forM_ [1..10] $ \i -> do
    echo "iteration"
    echo i
```

## Variable substitution

```bash
a=example
echo "$a.com"
```

```haskell
-- 
let a = example
echo (a ++ ".com")
```

NB: Or look at libraries like [interpolate](http://hackage.haskell.org/package/interpolate/docs/Data-String-Interpolate.html)
```haskell
let a = example
echo [i|#{a}.com|]
```
