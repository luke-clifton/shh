{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Hssh.TH where

import Control.Monad
import Hssh
import Language.Haskell.TH
import System.Process

loadPath :: String -> Q [Dec]
loadPath path =
    let
        impl = valD (varP (mkName path)) (normalB [|
            toArgs [] path 
            |]) []
        name = mkName path
        typn = mkName "a"
        typ = SigD (mkName path) (ForallT [PlainTV typn] [AppT (ConT ''Unit) (VarT typn), AppT (ConT ''ExecArgs) (VarT typn)] (VarT typn))
    in do
        i <- impl
        return $ [typ,i]

loadEnv :: Q [Dec]
loadEnv = do
    bins <- runIO pathBins
    fmap join $ mapM loadPath bins
