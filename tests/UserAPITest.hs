
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Data.Proxy
import GHC.TypeLits
import System.Exit

import Retcon.DataSource

instance RetconEntity "customer" where
    entitySource _ = [SomeDataSource (Proxy :: Proxy "directory")]

instance RetconDataSource "customer" "directory" where
    getDocument _key = error "Not implemented"
    setDocument _doc _key = error "Not implemented"
    deleteDocument _key = error "Not implemented"

-- | This test is mainly to make sure that the types line up in the
-- instances above.
main :: IO ()
main = do
    putStrLn "Type checker passes"
    putStrLn "The code compiles, links and runs"
    putStrLn "Surely it's correct"
    exitSuccess

