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

## Forking (async) processes

```bash
task_a &
p=$?
task_b
wait $p
```

Concurrency is pretty hard to do in a shell script in a robust way, so you
often see something like the above, where maybe `task_a` has started a
service that `task_b` needs to use. It is unusual for someone to put in the
effort of dealing with all the failure scenarios, as this gets pretty messy.

```haskell
import Control.Concurrent.Async

-- Very similar to the original shell script
withAsync task_a $ \p -> do
    task_b
    wait p

-- Waits for both processes to exit normally, but will terminate the other
-- process if something goes wrong, e.g. if task_a exits prematurely, task_b
-- will be cancelled.
concurrently_ task_a task_b

-- Waits for the first one to finish, and kills the other one as soon as that
-- happens. Say, if task_a is a webservice that task_b is going to use, but
-- we want to shut down that service once task_b is done. In the original
-- shell script, this would have been an action that task_b would have to
-- take explicitly, by sending a signal to the process.
race_ task_a task_b
```

## Reacting to failure

```bash
false || echo "It failed"
```

```haskell
false `catch` \Failure{} -> echo "It failed"
```

## Reacting to success

```bash
true && echo "It worked"
```

```haskell
-- Shh throws exceptions on failure, so it is implicit that the previous
-- command succeeded. So, we just sequence them.

true >> echo "It worked"

-- Or simply

true
echo "It worked"
```

## Heredocs

```bash
cat <<EOF
A here doc is here in the script, and
can span multiple lines.
EOF
```

```haskell
-- Using native Haskell string syntax.
cat <<<"\
\A here doc is here in the script, and\n\
\can span multiple lines.\n\
\"

-- Or, using an interpolation quasi-quoter library
cat <<< [r|
A here doc is here in the script, and
can span multiple lines.
|]
```
