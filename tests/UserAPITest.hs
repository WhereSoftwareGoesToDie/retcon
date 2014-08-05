
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}

module Main where

import Data.Proxy
import GHC.TypeLits
import System.Directory
import System.Exit
import System.FilePath

import Retcon.DataSource
import Retcon.DataSource.JsonDirectory

instance RetconEntity "customer" where
    entitySource _ = [
        SomeDataSource (Proxy :: Proxy "data"),
        SomeDataSource (Proxy :: Proxy "test-results")
        ]

instance RetconDataSource "customer" "data" where
    getDocument _key = do
        f <- dataFp
        res <- getJsonDirDocument f _key
        either (error . show) return res
    setDocument _doc _key = do
        f <- dataFp
        res <- setJsonDirDocument f _doc _key
        either (error . show) (\x ->
            case x of
                Nothing -> error "No key"
                Just y  -> return y) res
    deleteDocument _key = do
        f <- dataFp
        res <- deleteJsonDirDocument f _key
        either (error . show) return res

instance RetconDataSource "customer" "test-results" where
    getDocument _key = do
        f <- testResultsFp
        res <- getJsonDirDocument f _key
        either (error . show) return res
    setDocument _doc _key = do
        f <- testResultsFp
        res <- setJsonDirDocument f _doc _key
        either (error . show) (\x ->
            case x of
                Nothing -> error "No key"
                Just y  -> return y) res
    deleteDocument _key = do
        f <- testResultsFp
        res <- deleteJsonDirDocument f _key
        either (error . show) return res

-- | get source file path
dataFp :: IO FilePath
dataFp = do
    cwd <- getCurrentDirectory
    return $ joinPath [cwd, "tests", "data"]

-- | get target file path
testResultsFp :: IO FilePath
testResultsFp = do
    cwd <- getCurrentDirectory
    return $ joinPath [cwd, "tests", "test-results"]

-- | This test is mainly to make sure that the types line up in the
-- instances above.
main :: IO ()
main = do
    putStrLn "Type checker passes"
    putStrLn "The code compiles, links and runs"
    putStrLn "Surely it's correct"
    exitSuccess

