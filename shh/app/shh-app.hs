{-# LANGUAGE LambdaCase #-}
module Main where

import Shh
import System.IO
import System.Environment
import System.Exit
import System.IO.Temp
import System.Directory

defaultShell = "\
\{-# LANGUAGE TemplateHaskell #-}\n\
\module Shell where\n\
\import Shh\n\
\$(loadEnv SearchPath)\n\
\ "

defaultInitGhci = "\
\:seti -XNoOverloadedLists\n\
\import Shh\n\
\ "

extraInitGhci = "\
\import Shh.Prompt\n\
\:set prompt-function formatPrompt \"\\n\\ESC[1;32m[%u@%h:%w]λ \\ESC[0m\"\n\
\:set prompt-cont \"| \"\n\
\ "


defaultWrapper = "\
\#! /usr/bin/env sh\n\
\exec \"$@\"\n\
\ "

debug = putStrLn

doIfMissing :: FilePath -> IO () -> IO ()
doIfMissing fp a = do
    doesFileExist fp >>= \case
        True -> pure ()
        False -> a

writeIfMissing :: FilePath -> String -> IO ()
writeIfMissing fp s = doIfMissing fp (writeFile fp s)

main :: IO ()
main = do
    a <- getArgs
    shhDir <- lookupEnv "SHH_DIR" >>= \case
        Nothing -> lookupEnv "HOME" >>= \case
            Nothing -> error "Please specify HOME or SHH_DIR environment variables"
            Just h  -> pure $ h <> "/.shh"
        Just s -> pure s

    let
        wrapper :: String
        wrapper = shhDir <> "/wrapper"


    debug $ "Shh home is: " <> shhDir

    createDirectoryIfMissing False shhDir

    withCurrentDirectory shhDir $ do
        writeIfMissing "wrapper" defaultWrapper
        setPermissions "wrapper" $
            setOwnerExecutable True $
            setOwnerReadable True $
            setOwnerWritable True $
            emptyPermissions
        doIfMissing "init.ghci" $ do
            catchFailure (exe wrapper "ghc-pkg" "latest" "shh") >>= \case
                Left _ -> do
                    putStrLn "Please make the shh and shh-extras packages available in the shh"
                    putStrLn "environment (install it globally or modify the wrapper, see docs)."
                    putStrLn "Aborting"
                    exitFailure
                Right _ -> writeFile "init.ghci" defaultInitGhci
            catchFailure (exe wrapper "ghc-pkg" "latest" "shh-extras") >>= \case
                Left _ -> do
                    putStrLn "## WARNING ##########################################################"
                    putStrLn "# You do not have the shh-extras library installed, and so we are"
                    putStrLn "# generating a reduced functionality init.ghci file. To restore full"
                    putStrLn "# functionality, install shh-extras and re-generate your init.ghci"
                    putStrLn $ "# file by deleting " <> shhDir <> "/init.ghci and re-running shh"
                    putStrLn "#####################################################################"
                Right _ -> appendFile "init.ghci" extraInitGhci
        writeIfMissing "Shell.hs" defaultShell

    runProc $ mkProc' True wrapper ["ghci", "-ghci-script", shhDir <> "/init.ghci", shhDir <> "/Shell.hs"]

