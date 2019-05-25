{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import System.Directory
import Shh
import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Exception
import Control.Monad
import Data.Char
import Data.Word
import Control.Concurrent.Async
import System.IO

load SearchPath
    ["wc", "head", "tr", "echo", "cat", "true", "false", "mktemp", "sleep"
    , "rm", "printf", "xargs", "find"
    ]

main = do
    putStrLn "################################################"
    putStrLn " These tests require that certain binaries"
    putStrLn " exist on your $PATH. If you are getting"
    putStrLn " failures, please check that it's not because"
    putStrLn " they are missing."
    putStrLn "################################################"
    doctest ["--fast", "-isrc", "src/Shh/Internal.hs"]
    defaultMain tests

tests :: TestTree
tests = localOption (Timeout 4000000 "4s") $ testGroup "Tests" [unitTests, properties]

bytesToString :: [Word8] -> String
bytesToString = map (chr . fromIntegral)

properties :: TestTree
properties = testGroup "Properties"
    [ testProperty "trim = trim . trim" $ \l -> trim l == trim (trim l)
    , testProperty "encodeIdentifier creates a unique encoding"
        $ \(l1,l2) -> (encodeIdentifier l1 == encodeIdentifier l2) == (l1 == l2)
    , testProperty "writeOutput" $ \s -> ioProperty $ do
        let
            s' = bytesToString s
        k <- writeOutput s' |> capture
        pure $ s' === k
    , testProperty "pureProc id" $ \s -> ioProperty $ do
        let
            s' = bytesToString s
        k <- readProc $ s' >>> pureProc id
        pure $ s' === k
    , testProperty "pureProc (map toUpper)" $ \(ASCIIString s) -> ioProperty $ do
        k <- readProc $ s >>> pureProc (map toUpper)
        pure $ map toUpper s === k
    , testProperty "pureProc . const === writeOutput" $ \s -> ioProperty $ do
        let
            s' = bytesToString s
        a <- writeOutput s' |> capture
        b <- pureProc (const s') |> capture
        pure $ a === b
    , testProperty "writeOutput s |> capture >>= writeOutput |> capture === s"
        $ \(ASCIIString s) -> ioProperty $ do
            r <- writeOutput s |> capture >>= writeOutput |> capture
            pure $ r === s
    , testProperty "pureProc id === readInputP (\\s -> writeOutput s)"
        $ \(ASCIIString s) -> ioProperty $ do
            a <- writeOutput s |> pureProc id |> capture
            b <- writeOutput s |> readInputP (\s -> writeOutput s) |> capture
            pure $ a === b
    ]

withTmp :: (FilePath -> IO a) -> IO a
withTmp = bracket (readTrim mktemp) rm

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "Read stdout" $ do
        l <- readProc $ echo "test"
        l @?= "test\n"
    , testCase "Redirect to /dev/null" $ do
        l <- readProc $ echo "test" &> devNull
        l @?= ""
    , testCase "Redirct stderr" $ do
        l <- echo "test" &> StdErr |!> capture
        l @?= "test\n"
    , testCase "Redirect to file (Truncate)" $ withTmp $ \t -> do
        echo "test" &> Truncate t
        r <- readProc $ cat t
        "test\n" @?= r
    , testCase "Redirect to file (Append)" $ withTmp $ \t -> do
        echo "test" &> Truncate t
        echo "test" &> Append t
        r <- readProc $ cat t
        "test\ntest\n" @?= r
    , testCase "Long pipe" $ do
        r <- readProc $ echo "test" |> tr "-d" "e" |> tr "-d" "s"
        r @?= "tt\n"
    , testCase "Pipe stderr" $ replicateM_ 100 $ do
        r <- readProc $ echo "test" &> StdErr |!> cat
        r @?= "test\n"
    , testCase "Lazy read" $ replicateM_ 100 $ do
        withRead (cat "/dev/zero") $ \s -> do
            take 6 s @?= "\0\0\0\0\0\0"
    , testCase "Multiple outputs" $ do
        l <- readProc $ (echo (1 :: Int) >> echo (2 :: Int)) |> cat
        l @?= "1\n2\n"
    , testCase "Terminate upstream processes" $ do
        Left x <- catchFailure (mkProc "false" ["dummy"] |> (sleep 1 >> false "Didn't kill"))
        x @?= Shh.Failure "false" ["dummy"] 1
    , testCase "Write to process" $ withTmp $ \t -> do
        writeProc (cat &> Truncate t) "Hello"
        r <- readProc (cat t)
        r @?= "Hello"
        writeProc (cat &> Truncate t) "Goodbye"
        r <- readProc (cat t)
        r @?= "Goodbye"
    , testCase "apply" $ do
        r <- apply (tr "-d" "es") "test"
        r @?= "tt"
    , testCase "ignoreFailure" $ replicateM_ 30 $ do
        r <- readProc $ ignoreFailure false |> echo "Hello"
        r @?= "Hello\n"
    , testCase "Read failure" $ replicateM_ 30 $ do
        Left r <- catchFailure $ readProc $ false "dummy"
        r @?= Shh.Failure "false" ["dummy"] 1
    , testCase "Read failure chain start" $ replicateM_ 30 $ do
        Left r <- catchFailure $ readProc $ false "dummy" |> echo "test" |> true
        r @?= Shh.Failure "false" ["dummy"] 1
    , testCase "Read failure chain middle" $ replicateM_ 30 $ do
        Left r <- catchFailure $ readProc $ echo "test" |> false "dummy" |> true
        r @?= Shh.Failure "false" ["dummy"] 1
    , testCase "Read failure chain end" $ replicateM_ 30 $ do
        Left r <- catchFailure $ readProc $ echo "test" |> true |> false "dummy"
        r @?= Shh.Failure "false" ["dummy"] 1
    , testCase "Lazy read checks code" $ replicateM_ 30 $ do
        Left r <- catchFailure $ withRead (cat "/dev/urandom" |> false "dummy") $ pure . take 3
        r @?= Shh.Failure "false" ["dummy"] 1
    , testCase "Identifier odd chars" $ encodeIdentifier "1@3.-" @?= "_1'40'3''_"
    , testCase "Identifier make lower" $ encodeIdentifier "T.est" @?= "_T''est"
    , testCase "pureProc closes input" $ do
        r <- readProc $ cat "/dev/urandom" |> pureProc (const "test")
        r @?= "test"
    , testCase "pureProc closes output" $ do
        r <- readProc $ pureProc (const "test") |> cat
        r @?= "test"
    , testCase "pureProc doesn't close std handles" $ do
        runProc $ pureProc (const "")
        b <- hIsOpen stdin
        b @?= True
        b <- hIsOpen stdout
        b @?= True
        runProc $ pureProc (const "") &> StdErr
        b <- hIsOpen stderr
        b @?= True
    , testCase "pureProc sanity check" $ do
        r <- readProc $ printf "Hello" |> pureProc id |> cat
        r @?= "Hello"
    , testCase "bind nativeProc" $ do
        r <- writeOutput "te" >> writeOutput "st" |> capture
        r @?= "test"
    , testCase "stdin interleave capture" $ do
        r <- writeOutput "te" >> writeError "--" &!> devNull >> writeOutput "st" >> writeError "--" &!> devNull |> capture
        r @?= "test"
    , testCase "stderr interleave capture" $ do
        r <- writeOutput "--" &> devNull >> writeError "te" >> writeOutput "--" &> devNull >> writeError "st" |!> capture
        r @?= "test"
    , testCase "prefixLines" $ do
        r <- printf "Hello\\nWorld" |> prefixLines ":" |> capture
        r @?= ":Hello\n:World\n"
    , testCase "Bind in the middle" $ do
        l <- echo "a" |> prefixLines ":" >> echo "c" |> prefixLines ":" |> capture
        l @?= "::a\n:c\n"
    , testCase "writeOutput mimics printf" $ do
        l <- writeOutput "a\0b\0c" |> capture
        r <- printf "a\\0b\\0c" |> capture
        l @?= r
    , testCase "xargs1 printf" $ do
        a <- printf "a\\0b\\0c" |> xargs "--null" "-L1" "echo" |> capture
        b <- printf "a\\0b\\0c" |> xargs1 "\0" echo |> capture
        a @?= b
    , testCase "xargs1 writeOutput" $ do
        a <- writeOutput "a\0b\0c" |> xargs "--null" "-L1" "echo" |> capture
        b <- writeOutput "a\0b\0c" |> xargs1 "\0" echo |> capture
        a @?= b
    , testCase "fixity |>" $ do
        a <- echo "a" >> echo "b" >> echo "c" |> Main.head "-n" 2 |> capture
        b <- (echo "a" >> echo "b" >> echo "c") |> Main.head "-n" 2 |> capture
        a @?= b
    , testCase "pureProc . const === writeOutput (spam test)" $ replicateM_ 3000 $ do
        let
            s = ""
        a <- writeOutput s |> capture
        b <- pureProc (const s) |> capture
        a @?= b
    , testCase "complex example with intermediate handles (>BUFSIZ)" $ do
        let c = 20000000
        s <- readTrim $ cat "/dev/urandom" |> readInputP (\s -> writeOutput (map toUpper s) |> cat) |> Main.head "-c" c |> wc "-c"
        show c @?= s
    , testCase "subshells" $ do
      s <- readProc $ echo "ac" |> (cat >> echo "bc") |> tr "-d" "c"
      s @?= "a\nb\n"
    ]
