{-# LANGUAGE TemplateHaskell #-}
import System.Environment
import Shh

-- Because this is a script it is VERY convinient to just load the entire
-- environment. Though, name collisions can be a pain.
$(loadEnv Absolute)


main :: IO ()
main = do
    arg <- unwords <$> getArgs
    enc <- apply base32 arg
    ponysay enc
