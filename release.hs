#! /usr/bin/env runghc
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import System.FilePath.Glob
import Shh
import System.Exit
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as B

loadEnv SearchPath

hackageVersion :: String -> IO String
hackageVersion pkg =
    curl "-fsS" "-H" "Accept: application/json" ("https://hackage.haskell.org/package/" ++ pkg ++ "/preferred")
        |> jq "-r" ".\"normal-version\"[0]"
        |> (B.unpack <$> captureTrim)

localVersion :: String -> IO String
localVersion pkg = 
    grep "^version:" (pkg ++ "/" ++ pkg ++ ".cabal")
        |> tr "-d" "[:space:]"
        |> cut "-d:" "-f2"
        |> (B.unpack <$> captureTrim)

needsUpload :: String -> IO Bool
needsUpload pkg = do
    hv <- hackageVersion pkg
    lv <- localVersion pkg
    pure (lv /= hv)

upload :: String -> IO ()
upload pkg = do
    needsUpload pkg >>= \case
        True -> do
            sanity pkg >>= \case
                True -> do
                    lv <- localVersion pkg
                    let toupload = pkg ++ "-" ++ lv
                    putStrLn $ "Uploading " ++ toupload
                    putStrLn "continue? (type 'yes')"
                    getLine >>= \case
                        "yes" -> pure ()
                        _     -> exitFailure
                    bracket
                        (mktemp "-d" |> captureTrim)
                        (rm "-Rf" "--")
                        $ \tmp -> do
                            cabal "new-sdist" "--verbose=0" "--builddir" tmp pkg
                            cabal "new-haddock" "--haddock-for-hackage" "--builddir" tmp pkg
                            cabal "upload" "--publish" (B.unpack tmp ++ "/sdist/" ++ toupload ++ ".tar.gz")
                            cabal "upload" "--publish" "-d" (B.unpack tmp ++ "/" ++ toupload ++ "-docs.tar.gz")
                            git "tag" "-a" toupload "-m" ("Releasing " ++ toupload)
                            git "push" "origin" toupload
                False -> putStrLn $ pkg ++ " has issues, not uploading"
        False -> do
            putStrLn $ pkg ++ " was not uploaded (versions match)"
            codeMatch pkg >>= \case
                True -> pure ()
                False -> do
                    putStrLn "... but there is a difference in the tarballs!"
                    putStrLn "... bump the version number to upload!"
                    putStrLn "... NB: this might differ because of metadata revisions."

codeMatch :: String -> IO Bool
codeMatch pkg = do
    bracket
        (mktemp "-d" |> captureTrim)
        (rm "-Rf" "--")
        $ \tmp -> do
            hv <- hackageVersion pkg
            cabal "new-sdist" "--verbose=0" "--builddir" tmp pkg
            src <- glob (B.unpack tmp ++ "/sdist/*.tar.gz")
            gunzip src
            src <- glob (B.unpack tmp ++ "/sdist/*.tar")
            mkdir "-p" (tmp <> "/local")
            mkdir "-p" (tmp <> "/remote")
            tar "-C" (tmp <> "/local") "-xf" src
            curl "-fsS"
                ("https://hackage.haskell.org/package/" ++ pkg ++ "/" ++ pkg ++ "-" ++ hv ++ ".tar.gz")
                |> gunzip
                |> tar "-C" (tmp <> "/remote") "-x"

            curl "-fsS"
                ("https://hackage.haskell.org/package/" ++ pkg ++ "-" ++ hv ++ "/" ++ pkg ++ ".cabal")
                |> ignoreFailure (grep "-v" "x-revision")
                &> Truncate (B.pack $ B.unpack tmp ++ "/remote/" ++ pkg ++ "-" ++ hv ++ "/" ++ pkg ++ ".cabal")

            translateCode (\case
                1 -> Just False
                _ -> Nothing
                ) $ const True <$> diff "--strip-trailing-cr" "-r" (tmp <> "/local") (tmp <> "/remote")


sanity :: String -> IO Bool
sanity pkg = tryFailure (do
    git "diff" "--exit-code"
    git "diff" "--cached" "--exit-code"
    cabal "new-build" pkg
    cabal "new-test" pkg
    gr <- cabal "new-haddock" "--haddock-for-hackage" pkg |> exitCode (grep "-A" 5 "Missing documentation")
    pure (gr == 0)
    ) >>= either (const $ pure False) pure

main :: IO ()
main = do
    upload "shh"
    upload "shh-extras"

