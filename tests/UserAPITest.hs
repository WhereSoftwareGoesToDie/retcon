
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Test.Hspec

import Control.Monad.IO.Class
import Data.Monoid
import Data.Proxy
import GHC.TypeLits
import System.Directory
import System.Exit
import System.FilePath

import Retcon.DataSource
import Retcon.DataSource.JsonDirectory
import Retcon.Error
import Retcon.Handler
import Retcon.Monad
import Retcon.Options
import Retcon.Store
import Retcon.Store.Memory

import TestHelpers

-- | Path to the "customer:data" JSON files.
customerDataPath :: FilePath
customerDataPath = joinPath ["tests", "data"]

-- | Path to the "customer:test-results" JSON files.
customerTestResultsPath :: FilePath
customerTestResultsPath = joinPath ["tests", "test-results"]

instance RetconEntity "customer" where
    entitySources _ = [
        SomeDataSource (Proxy :: Proxy "data"),
        SomeDataSource (Proxy :: Proxy "test-results")
        ]

instance RetconDataSource "customer" "data" where

    data DataSourceState "customer" "data" = Nowt

    initialiseState = return Nowt

    finaliseState Nowt = return ()

    getDocument key = liftIO $ do
        res <- getJsonDirDocument customerDataPath key
        either (error . show) return res

    setDocument doc key = liftIO $ do
        res <- setJsonDirDocument customerDataPath doc key
        either (error . show) (\x ->
            case x of
                Nothing -> error "No key"
                Just y  -> return y) res

    deleteDocument key = liftIO $ do
        res <- deleteJsonDirDocument customerDataPath key
        either (error . show) return res

instance RetconDataSource "customer" "test-results" where

    data DataSourceState "customer" "test-results" = Blarg

    initialiseState = return Blarg

    finaliseState Blarg = return ()

    getDocument key = liftIO $ do
        res <- getJsonDirDocument customerTestResultsPath key
        either (error . show) return res

    setDocument doc key = liftIO $ do
        res <- setJsonDirDocument customerTestResultsPath doc key
        either (error . show) (\x ->
            case x of
                Nothing -> error "No key"
                Just y  -> return y) res

    deleteDocument key = liftIO $ do
        res <- deleteJsonDirDocument customerTestResultsPath key
        either (error . show) return res

run :: l -> RetconMonad ROToken l r -> IO (Either RetconError r)
run l a = do
    store <- storeInitialise opt :: IO MemStorage
    result <- runRetconMonad opt state (restrictToken . token $ store) l a
    storeFinalise store
    return result
  where
    opt = defaultOptions
    state = []


-- | test suite
suite :: Spec
suite =
    describe "JSON directory marshalling" $ do
        it "can load 01-diff-source" $ do
            state <- initialiseState
            result <- run state $
                getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "data")
            finaliseState state
            case result of
                Left _ -> error "Could not get document for 01-diff-source!"
                Right _ -> return ()

        it "can load 01-diff-target" $ do
            state <- initialiseState
            result <- run state $
                getDocument (ForeignKey "01-diff-target" :: ForeignKey "customer" "data")
            finaliseState state
            case result of
                Left _ -> error "Could not get document for 01-diff-target!"
                Right _ -> return ()

        it "can write 01-diff-source to another source with that key" $ do
            state1 <- initialiseState
            Right doc3 <- run state1 $
                getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "data")
            finaliseState state1

            let fk = ForeignKey "01-diff-source" :: ForeignKey "customer" "test-results"
            state2 <- initialiseState
            result <- run state2 $
                setDocument doc3 (Just fk)
            finaliseState state2
            result `shouldBe` Right fk

        it "can write 01-diff-source to another source with new key" $ do
            state <- initialiseState
            Right doc4 <- run state $
                getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "data")
            finaliseState state

            state2 <- initialiseState
            _ <- run state2 $
                setDocument doc4 (Nothing :: Maybe (ForeignKey "customer" "test-results"))
            finaliseState state2
            pass

        it "can delete 01-diff-source from the test source" $ do
            state <- initialiseState
            _ <- run state $ do
                _ <- getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "test-results")
                deleteDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "test-results")
            finaliseState state
            pass

-- | This test is mainly to make sure that the types line up in the
-- instances above.
main :: IO ()
main = do
    createDirectoryIfMissing True customerTestResultsPath
    hspec suite
  where
    cfg = RetconConfig [ SomeEntity (Proxy :: Proxy "customer") ]

