--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

-- | Test the API round-trip.
module Main where

import Control.Concurrent.Async
import Control.Exception
import Data.ByteString ()
import qualified Data.ByteString.Char8 as BS
import System.Process
import Test.Hspec

import Retcon.Network.Client
import Retcon.Network.Server
import Retcon.Options
import Retcon.Store.PostgreSQL

prepareDatabase :: BS.ByteString -> IO () -> IO ()
prepareDatabase dbname action = bracket setupSuite teardownSuite (const action)
  where
    db = BS.unpack dbname

    setupSuite :: IO ()
    setupSuite = do
        _ <- system $ concat [ " dropdb --if-exists ", db, " >/dev/null 2>&1 "
                             , " && createdb ", db
                             , " && psql --quiet --file=retcon.sql ", db
                             ]
        return ()

    teardownSuite :: a -> IO ()
    teardownSuite _ = do
        _ <- system $ concat [ "dropdb --if-exists ", db, " >/dev/null 2>&1 " ]
        return ()

suite :: String -> BS.ByteString -> Spec
suite conn dbname = around (prepareDatabase dbname) $ describe "Retcon API" $ do
    it "replies to conflict list requests" $ do
        result <- runRetconZMQ conn getConflicted
        result `shouldBe` Right []

    it "replies to resolve conflict requests" $ do
        let diff_id = DiffID 1
        let ops = []
        result <- runRetconZMQ conn $ enqueueResolveDiff diff_id ops
        result `shouldBe` Right ()

    it "replies to notify requests" $ do
        let note = ChangeNotification "TestEntity" "TestSource" "item1"
        result <- runRetconZMQ conn $ enqueueChangeNotification note
        result `shouldBe` Right ()

    it "replies to invalid requests" $
        -- Right result <- runRetconZMQ conn $ performRequest InvalidHeader
        -- result `shouldbe` InvalidResponse
        pendingWith "This cannot be implemented without changing the interface."

main :: IO ()
main = do
    let conn = "tcp://127.0.0.1:1234"
    let db = "dbname=retcon_test"
    let entities = []

    -- Prepare the retcon and server configurations.
    let serverConfig = ServerConfig conn
    let retconOpt = RetconOptions False LogStderr db Nothing
    retconConfig <- prepareConfig (retconOpt, []) entities

    -- Spawn the server.
    server <- async $ apiServer retconConfig serverConfig

    -- Run the test suite.
    hspec (suite conn db)

    -- Shut the server down.
    cancel server
