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
