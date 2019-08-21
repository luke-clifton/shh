{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import System.Directory
import Shh
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import qualified Data.ByteString.Lazy.Char8 as C8
import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Exception
import Control.Monad
import Data.Char
import Data.Word
import Control.Concurrent.Async
import System.FilePath (takeFileName)
import System.IO

import Readme

load SearchPath
    [ "wc", "head", "tr", "echo", "cat", "true", "false", "mktemp", "sleep"
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
tests = localOption (Timeout 8000000 "8s") $ testGroup "Tests" [unitTests, properties]

bytesToString :: [Word8] -> ByteString
bytesToString = BS.pack

instance Arbitrary ByteString where
    arbitrary = bytesToString <$> arbitrary
    shrink = fmap BS.pack . shrink . BS.unpack

properties :: TestTree
properties = testGroup "Properties"
    [ testProperty "trim = trim . trim" $ \l -> trim l == trim (trim l)
    , testProperty "encodeIdentifier creates a unique encoding"
        $ \(l1,l2) -> (encodeIdentifier l1 == encodeIdentifier l2) == (takeFileName l1 == takeFileName l2)
    , testProperty "writeOutput" $ \s -> ioProperty $ do
        let
            s' = bytesToString s
        k <- writeOutput s' |> capture
        pure $ s' === k
    , testProperty "pureProc id" $ \s -> ioProperty $ do
        let
            s' = bytesToString s
        k <- apply (pureProc id) s'
        pure $ s' === k
    , testProperty "pureProc (map toUpper)" $ \s -> ioProperty $ do
        k <- apply (pureProc (C8.map toUpper)) s
        pure $ C8.map toUpper s === k
    , testProperty "pureProc . const === writeOutput" $ \s -> ioProperty $ do
        let
            s' = bytesToString s
        a <- writeOutput s' |> capture
        b <- pureProc (const s') |> capture
        pure $ a === b
    , testProperty "writeOutput s |> capture >>= writeOutput |> capture === s"
        $ \s -> ioProperty $ do
            r <- writeOutput s |> capture >>= writeOutput |> capture
            pure $ r === s
    , testProperty "pureProc id === readInputP (\\s -> writeOutput s)"
        $ \s -> ioProperty $ do
            a <- writeOutput s |> pureProc id |> capture
            b <- writeOutput s |> readInputP (\s -> writeOutput s) |> capture
            pure $ a === b
    , testProperty "string round trip" $ \s -> ioProperty $ do
        r <- writeOutput (s :: String) |> capture
        pure $ s == toString r
    , testProperty "bytestring round trip" $ \s -> ioProperty $ do
        r <- writeOutput (s :: ByteString) |> capture
        pure $ s == r
    ]

withTmp :: (ByteString -> IO a) -> IO a
withTmp = bracket (mktemp |> captureTrim) rm

checkFailure :: Failure -> ByteString -> [ByteString] -> Int -> IO ()
checkFailure f prog args code = do
    failureProg f @?= prog
    failureArgs f @?= args
    failureCode f @?= code

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "Read stdout" $ do
        l <- echo "test" |> capture
        l @?= "test\n"
    , testCase "Redirect to /dev/null" $ do
        l <- echo "test" &> devNull |> capture
        l @?= ""
    , testCase "Redirct stderr" $ do
        l <- echo "test" &> StdErr |!> capture
        l @?= "test\n"
    , testCase "Redirect to file (Truncate)" $ withTmp $ \t -> do
        echo "test" &> Truncate t
        r <- cat t |> capture
        "test\n" @?= r
    , testCase "Redirect to file (Append)" $ withTmp $ \t -> do
        echo "test" &> Truncate t
        echo "test" &> Append t
        r <- cat t |> capture
        "test\ntest\n" @?= r
    , testCase "Long pipe" $ do
        r <- echo "test" |> tr "-d" "e" |> tr "-d" "s" |> capture
        r @?= "tt\n"
    , testCase "Pipe stderr" $ replicateM_ 100 $ do
        r <- echo "test" &> StdErr |!> cat |> capture
        r @?= "test\n"
    , testCase "Lazy read" $ replicateM_ 100 $ do
        cat "/dev/zero" |> readInput (\s -> do
            BS.take 6 s @?= "\0\0\0\0\0\0"
            )
    , testCase "Multiple outputs" $ do
        l <- (echo (1 :: Int) >> echo (2 :: Int)) |> cat |> capture
        l @?= "1\n2\n"
    , testCase "Terminate upstream processes" $ do
        Left x <- tryFailure (mkProc "false" ["dummy"] |> (sleep 1 >> false "Didn't kill"))
        checkFailure x "false" ["dummy"] 1
    , testCase "Write to process" $ withTmp $ \t -> do
        writeProc (cat &> Truncate t) "Hello"
        r <- cat t |> capture
        r @?= "Hello"
        writeProc (cat &> Truncate t) "Goodbye"
        r <- cat t |> capture
        r @?= "Goodbye"
    , testCase "apply" $ do
        r <- apply (tr "-d" "es") "test"
        r @?= "tt"
    , testCase "ignoreFailure" $ replicateM_ 30 $ do
        r <- ignoreFailure false |> echo "Hello" |> capture
        r @?= "Hello\n"
    , testCase "Read failure" $ replicateM_ 30 $ do
        Left r <- tryFailure $ false "dummy" |> capture
        checkFailure r "false" ["dummy"] 1
    , testCase "Read failure chain start" $ replicateM_ 30 $ do
        Left r <- tryFailure $ false "dummy" |> echo "test" |> true |> capture
        checkFailure r "false" ["dummy"] 1
    , testCase "Read failure chain middle" $ replicateM_ 30 $ do
        Left r <- tryFailure $ echo "test" |> false "dummy" |> true |> capture
        checkFailure r "false" ["dummy"] 1
    , testCase "Read failure chain end" $ replicateM_ 30 $ do
        Left r <- tryFailure $ echo "test" |> true |> false "dummy" |> capture
        checkFailure r "false" ["dummy"] 1
    , testCase "Lazy read checks code" $ replicateM_ 30 $ do
        Left r <- tryFailure $ cat "/dev/urandom" |> false "dummy" |> readInput (pure . BS.take 3)
        checkFailure r "false" ["dummy"] 1
    , testCase "Identifier odd chars" $ encodeIdentifier "1@3.-" @?= "_1'40'3''_"
    , testCase "Identifier make lower" $ encodeIdentifier "T.est" @?= "_T''est"
    , testCase "pureProc closes input" $ do
        r <- cat "/dev/urandom" |> pureProc (const "test") |> capture
        r @?= "test"
    , testCase "pureProc closes output" $ do
        r <- pureProc (const "test") |> cat |> capture
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
        r <- printf "Hello" |> pureProc id |> cat |> capture
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
        s <- cat "/dev/urandom" |> readInputP (\s -> writeOutput (C8.map toUpper s) |> cat) |> Main.head "-c" c |> wc "-c" |> captureTrim
        show c @?= toString s
    , testCase "subshells" $ do
      s <- echo "ac" |> (cat >> echo "bc") |> tr "-d" "c" |> capture
      s @?= "a\nb\n"
    , testCase "unicode" $ do
        s <- writeOutput "üか" |> cat |> capture
        toString s @?= "üか"
    , testCase "tryFailure capture 1" $ do
        s <- (tryFailure (false |> capture)) |> capture
        s @?= ""
    , testCase "tryFailure capture 2" $ do
        s <- (tryFailure (false |> capture)) |!> capture
        s @?= ""
    , testCase "tryFailure capture 3" $ do
        s <- (tryFailure (false |!> capture)) |> capture
        s @?= ""
    , testCase "tryFailure capture 4" $ do
        s <- (tryFailure (false |!> capture)) |!> capture
        s @?= ""
    , testCase "pipeErr" $ do
        s <- writeOutput "abcd" |> capture `pipeErr` capture
        s @?= ("abcd", "")
    , testCase "pipe" $ do
        s <- writeOutput "abcd" |> capture `pipe` capture
        s @?= ("abcd", "")
    ]
