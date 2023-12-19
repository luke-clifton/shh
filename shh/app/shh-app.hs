{-# LANGUAGE LambdaCase #-}
module Main where

import Control.DeepSeq (force)
import Control.Exception
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString,pack)
import Shh
import System.IO
import System.IO.Error
import System.Environment
import System.Exit
import System.IO.Temp
import System.Directory
import System.Posix.Process

defaultShell = "\
\{-# LANGUAGE TemplateHaskell #-}\n\
\module Shell where\n\
\import Shh\n\
\$(loadEnv SearchPath)\n\
\"

defaultInitGhci = "\
\:seti -XNoOverloadedLists\n\
\import Shh\n\
\"

extraInitGhci = "\
\import Shh.Prompt\n\
\:set prompt-function formatPrompt \"\\n\\ESC[1;32m[%u@%h:%w]λ \\ESC[0m\"\n\
\:set prompt-cont \"| \"\n\
\"


defaultWrapper = "\
\#! /usr/bin/env sh\n\
\exec \"$@\"\n\
\"

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

    let debug = elem "--debug" a
    when debug $ putStrLn $ "Shh home is: " <> shhDir

    case a of
        ["--rebuild"] -> do
            removeFile "Shell.hi"
            removeFile "Shell.o"
        ["--help"] -> do
            putStrLn "usage: shh [--rebuild][--debug]"
            exitSuccess
        ["--debug"] -> pure ()
        [] -> pure ()
        _ -> error $ "Unknown arguments: " ++ show a

    createDirectoryIfMissing False shhDir

    withCurrentDirectory shhDir $ do
        writeIfMissing "wrapper" defaultWrapper
        setPermissions "wrapper" $
            setOwnerExecutable True $
            setOwnerReadable True $
            setOwnerWritable True $
            emptyPermissions
        doIfMissing "init.ghci" $ do
            putStrLn "Generating init.ghci..."
            putStrLn " ... checking for shh..."
            tryFailure (exe (pack wrapper) "ghc" (if debug then "" else "-v0") "-e" "import Shh") >>= \case
                Left _ -> do
                    putStrLn "Please make the shh and shh-extras packages available in the shh"
                    putStrLn "environment (install it globally or modify the wrapper, see docs)."
                    putStrLn "Aborting"
                    exitFailure
                Right _ -> writeFile "init.ghci" defaultInitGhci
            putStrLn " ... checking for shh-extras..."
            tryFailure (exe (pack wrapper) "ghc" (if debug then "" else "-v0") "-e" "import Shh.Prompt") >>= \case
                Left _ -> do
                    putStrLn "## WARNING ##########################################################"
                    putStrLn "# You do not have the shh-extras library installed, and so we are"
                    putStrLn "# generating a reduced functionality init.ghci file. To restore full"
                    putStrLn "# functionality, install shh-extras and re-generate your init.ghci"
                    putStrLn $ "# file by deleting " <> shhDir <> "/init.ghci and re-running shh"
                    putStrLn "#####################################################################"
                Right _ -> appendFile "init.ghci" extraInitGhci
        writeIfMissing "Shell.hs" defaultShell
        writeIfMissing "paths" ""
        pp <- readFile "paths"
        cp <- show <$> pathBins
        pathDiff <- evaluate $ force pp /= cp
        outdated <- catch (do
            shellMod <- getModificationTime "Shell.hs"
            hiMod    <- getModificationTime "Shell.hi"
            pure (shellMod > hiMod)
            ) (\e -> if isDoesNotExistError e then pure True else throwIO e)
        when (outdated || pathDiff) $ do
            putStrLn "Rebuilding Shell.hs..."
            writeFile "paths" cp
            -- Use absolute path of Shell.hs so that GHCi doesn't recompile.
            exe (pack wrapper) "ghc" "-c" "-dynamic" (shhDir <> "/Shell.hs") (if debug then "" else "-v0")

    executeFile wrapper False (["ghci", "-ghci-script", shhDir <> "/init.ghci", shhDir <> "/Shell.hs"] <> if debug then [] else ["-v0"]) Nothing

