--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Test the API round-trip.
module Main where

import Data.Monoid
import System.Process
import System.Exit
import qualified Data.List as L
import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Except
import           Data.Configurator
import           Data.Either.Combinators
import           System.Log.Logger
import           Test.Hspec

import           Synchronise
import           Synchronise.Network.Client
import           Synchronise.Network.Protocol
import           Synchronise.Network.Server

path   = "test/api-roundtrip-test.conf"
dbconn = "tcp://127.0.0.1:1234"
db     = "synchronised_test"
dbfile = "test/synchronised.sql"

--------------------------------------------------------------------------------

suite :: String -> Spec
suite conn = describe "Synchronise API" $ do
    it "replies to conflict list requests" $ do
        result <- runSynchroniseZMQ conn getConflicted
        result `shouldBe` Right []

    it "replies to resolve conflict requests" $ do
        let diff_id = 1
        let ops     = []
        result <- runSynchroniseZMQ conn $ enqueueResolvePatch diff_id ops
        result `shouldBe` Right ()

    it "replies to 'bad entity' notify requests with error" $ do
        let note = ChangeNotification "BadEntity" "BadSource" "item1"
        result <- runSynchroniseZMQ conn $ enqueueChangeNotification note
        result `shouldBe` Left UnknownKeyError

    it "replies to 'bad source' notify requests with error" $ do
        let note = ChangeNotification "TestEntity" "BadSource" "item1"
        result <- runSynchroniseZMQ conn $ enqueueChangeNotification note
        result `shouldBe` Left UnknownKeyError

    it "replies to good notify requests with success" $ do
        let note = ChangeNotification "TestEntity" "TestSource" "item1"
        result <- runSynchroniseZMQ conn $ enqueueChangeNotification note
        result `shouldBe` Right ()

    it "replies to invalid requests" $
        -- Right result <- runSynchroniseZMQ conn $ performRequest InvalidHeader
        -- result `shouldbe` InvalidResponse
        pendingWith "This cannot be implemented without changing the interface."

-- | Testing only, no input sanitisation.
--
resetTestDB :: String -> FilePath -> IO ()
resetTestDB d fixture = do
  x <- system . L.intercalate " && " $
      [ "dropdb --if-exists " <> d
      , "createdb " <> d
      , "psql " <> d <> " -f " <> fixture
      ]
  case x of
    ExitSuccess   -> return ()
    ExitFailure n -> error $ "Failed to reset test db, cmd failed with " <> show n

main :: IO ()
main = do
    cfg    <-  fmap (fromRight (error "cannot load test config")) . join
            $  runExceptT
           <$> parseConfiguration
           <$> load [Required path]

    resetTestDB db dbfile
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    server <- async (spawnServer cfg 1)

    hspec (suite dbconn)

    cancel server
