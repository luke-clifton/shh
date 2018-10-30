{-# LANGUAGE TemplateHaskell #-}
module Main where

import Hssh
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

$(loadEnv)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties"
    [ testProperty "trim = trim . trim" $ \l -> trim l == trim (trim l)
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "Read stdout" $ do
        l <- readProc $ echo "test"
        l @?= "test\n"
    , testCase "Redirect to /dev/null" $ do
        l <- readProc $ echo "test" &> Append "/dev/null"
        l @?= ""
    , testCase "Redirect to file (Truncate)" $ do
        t <- trim <$> readProc mktemp
        echo "test" &> Truncate t
        r <- readProc $ cat t
        rm t
        "test\n" @?= r
    , testCase "Redirect to file (Append)" $ do
        t <- readTrim mktemp
        echo "test" &> Truncate t
        echo "test" &> Append t
        r <- readProc $ cat t
        rm t
        "test\ntest\n" @?= r
    , testCase "Long pipe" $ do
        r <- readProc $ echo "test" |> shasum |> shasum |> shasum
        r @?= "3f18e7bc4021e72e52fc1395ece85d82f912a74a  -\n"
    , testCase "Pipe stderr" $ do
        r <- readProc $ echo "test" &> StdErr |!> cat
        r @?= "test\n"
    , testCase "Lazy read" $ do
        withRead (cat "/dev/urandom" |> xxd) $ \s -> do
            take 10 s @?= "00000000: "
    , testCase "Multiple outputs" $ do
        l <- readProc $ (echo (1 :: Int) >> echo (2 :: Int)) |> cat
        l @?= "1\n2\n"
    , testCase "Terminate upstream processes" $ do
        Left x <- catchFailure (mkProc "false" ["dummy"] |> yes "Terminate upstream process failed")
        x @?= Hssh.Failure "false" ["dummy"] 1
    , testCase "Write to process" $ do
        t <- readTrim mktemp
        writeProc (cat &> Truncate t) "Hello"
        r <- readProc (cat t)
        r @?= "Hello"
        writeProc (cat &> Truncate t) "Goodbye"
        r <- readProc (cat t)
        r @?= "Goodbye"
    ]
