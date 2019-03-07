module Shh.Prompt where

import Shh
import System.Environment
import Network.HostName

type PromptFn = [String] -> Int -> IO String

formatPrompt :: String -> PromptFn
formatPrompt fmt _ _ = do
    initInteractive
    format fmt
        where

            format :: String -> IO String
            format ('%' : '%' : rest) = ('%':) <$> format rest
            format ('%' : 'u' : rest) = insertEnv "USER" rest
            format ('%' : 'w' : rest) = insertEnv "PWD" rest
            format ('%' : 'h' : rest) = insertIO getHostName rest
            format ( x  : rest) = (x:) <$> format rest
            format [] = pure []

            insertEnv :: String -> String -> IO String
            insertEnv var rest = insertIO (getEnv var) rest

            insertIO :: IO String -> String -> IO String
            insertIO a rest = a >>= \s -> (s ++) <$> format rest
