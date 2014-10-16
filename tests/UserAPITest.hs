
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Hspec

import Control.Applicative
import Control.Lens.Operators
import Data.Monoid
import Data.Proxy
import System.Directory
import System.FilePath

import Retcon.DataSource
import Retcon.DataSource.JsonDirectory
import Retcon.Error
import Retcon.Monad
import Retcon.Options
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

    data DataSourceState "customer" "data" = Nowt FilePath

    initialiseState = Nowt <$> testJSONFilePath
    finaliseState _ = return ()

    getDocument key =
        getActionState >>= \(Nowt fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(Nowt fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(Nowt fp) -> deleteJSONDir fp key

instance RetconDataSource "customer" "test-results" where

    data DataSourceState "customer" "test-results" = Blarg FilePath

    getDocument key =
        getActionState >>= \(Blarg fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(Blarg fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(Blarg fp) -> deleteJSONDir fp key

    initialiseState = Blarg <$> testJSONFilePath
    finaliseState _ = return ()

run :: l -> RetconMonad InitialisedEntity ROToken l r -> IO (Either RetconError r)
run l a = do
    store <- storeInitialise opt :: IO MemStorage
    let store' = restrictToken . token $ store
    let cfg = RetconConfig
                (opt ^. optVerbose)
                (opt ^. optLogging)
                store'
                mempty
                (opt ^. optArgs)
                state
    result <- runRetconMonad (RetconMonadState cfg l) a
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
            state <- runInitialiser mempty initialiseState
            result <- run state $
                getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "data")
            runInitialiser mempty $ finaliseState state
            case result of
                Left _ -> error "Could not get document for 01-diff-source!"
                Right _ -> return ()

        it "can load 01-diff-target" $ do
            state <- runInitialiser mempty initialiseState
            result <- run state $
                getDocument (ForeignKey "01-diff-target" :: ForeignKey "customer" "data")
            runInitialiser mempty $ finaliseState state
            case result of
                Left _ -> error "Could not get document for 01-diff-target!"
                Right _ -> return ()

        it "can write 01-diff-source to another source with that key" $ do
            state1 <- runInitialiser mempty initialiseState
            Right doc3 <- run state1 $
                getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "data")
            runInitialiser mempty $ finaliseState state1

            let fk = ForeignKey "01-diff-source" :: ForeignKey "customer" "test-results"
            state2 <- runInitialiser mempty initialiseState
            result <- run state2 $
                setDocument doc3 (Just fk)
            runInitialiser mempty $ finaliseState state2
            result `shouldBe` Right fk

        it "can write 01-diff-source to another source with new key" $ do
            state <- runInitialiser mempty initialiseState
            Right doc4 <- run state $
                getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "data")
            runInitialiser mempty $ finaliseState state

            state2 <- runInitialiser mempty initialiseState
            _ <- run state2 $ do
                k <- setDocument doc4 (Nothing :: Maybe (ForeignKey "customer" "test-results"))
                deleteDocument k
            runInitialiser mempty $ finaliseState state2
            pass

        it "can delete 01-diff-source from the test source" $ do
            state <- runInitialiser mempty initialiseState
            _ <- run state $ do
                _ <- getDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "test-results")
                deleteDocument (ForeignKey "01-diff-source" :: ForeignKey "customer" "test-results")
            runInitialiser mempty $ finaliseState state
            pass

-- | This test is mainly to make sure that the types line up in the
-- instances above.
main :: IO ()
main = do
    createDirectoryIfMissing True customerTestResultsPath
    hspec suite
