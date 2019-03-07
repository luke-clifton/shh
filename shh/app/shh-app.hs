{-# LANGUAGE LambdaCase #-}
module Main where

import Shh
import System.IO
import System.Environment
import System.IO.Temp
import System.Directory
import Data.Hashable (hash)
import Data.List.Split (splitOn)

defaultShell = "\
\{-# LANGUAGE TemplateHaskell #-}\n\
\module Shell where\n\
\import Shh\n\
\$(loadEnv SearchPath)\n\
\ "

defaultInitGhci = "\
\:seti -XNoOverloadedLists\n\
\import Shh\n\
\import Shh.Prompt\n\
\:set prompt-function promptFormat \"\\n\\ESC[1;32m[%u@%h:%w]λ \\ESC[0m\"\n\
\ "


defaultWrapper = "\
\#! /usr/bin/env sh\n\
\exec \"$@\"\n\
\ "

debug = putStrLn

writeIfMissing :: FilePath -> String -> IO ()
writeIfMissing fp s = do
    doesFileExist fp >>= \case
        True -> pure ()
        False -> writeFile fp s

main :: IO ()
main = do
    a <- getArgs
    shhDir <- lookupEnv "SHH_DIR" >>= \case
        Nothing -> lookupEnv "HOME" >>= \case
            Nothing -> error "Please specify HOME or SHH_DIR environment variables"
            Just h  -> pure $ h <> "/.shh"
        Just s -> pure s

    let
        wrapped :: (Unit a, ExecArgs a) => a
        wrapped = exe (shhDir <> "/wrapper")


    debug $ "Shh home is: " <> shhDir

    createDirectoryIfMissing False shhDir

    withCurrentDirectory shhDir $ do
        writeIfMissing "init.ghci" defaultInitGhci
        writeIfMissing "wrapper" defaultWrapper
        setPermissions "wrapper" $
            setOwnerExecutable True $
            setOwnerReadable True $
            setOwnerWritable True $
            emptyPermissions
        writeIfMissing "Shell.hs" defaultShell

    wrapped "ghci" "-ghci-script" (shhDir <> "/init.ghci") (shhDir <> "/Shell.hs")

