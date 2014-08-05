
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}

module Main where

import Test.Hspec

import Data.Proxy
import GHC.TypeLits
import System.Directory
import System.Exit
import System.FilePath

import Retcon.DataSource
import Retcon.DataSource.JsonDirectory

import TestHelpers

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

-- | test suite
suite :: Spec
suite = do
    describe "JSON directory marshalling" $ do
        it "can load 01-diff-source" $ do
            test1Doc <- getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "data")
            pass

        it "can load 01-diff-target" $ do
            test2Doc <- getDocument (ForeignKey "01-diff-target" :: ForeignKey "customer" "data")
            pass

        it "can write 01-diff-source to another source with that key" $ do
            doc3 <- getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "data")
            test3Key <- setDocument doc3 (Just (ForeignKey "01-diff-source" :: ForeignKey "customer" "test-results"))
            pass

        it "can write 01-diff-source to another source with new key" $ do
            doc4 <- getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "data")
            test4Key <- setDocument doc4 (Nothing :: Maybe (ForeignKey "customer" "test-results"))
            pass

        it "can delete 01-diff-source from the test source" $ do
            doc5 <- getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "test-results")
            test5DeleteOK <- deleteDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "test-results")
            pass

-- | This test is mainly to make sure that the types line up in the
-- instances above.
main :: IO ()
main = do
    putStrLn "Type checker passes"
    putStrLn "The code compiles, links and runs"
    putStrLn "Surely it's correct"
    hspec suite





