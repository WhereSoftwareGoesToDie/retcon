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
import Data.ByteString ()
import Test.Hspec

import Retcon.Network.Client
import Retcon.Network.Server
import Retcon.Options
import Retcon.Store.PostgreSQL

suite :: String -> Spec
suite conn = describe "Retcon API" $ do
    it "replies to conflict list requests" $ do
        Right result <- runRetconZMQ conn getConflicted
        result `shouldBe` []

    it "replies to resolve conflict requests" $ do
        let diff_id = DiffID 1
        let ops = []
        Right result <- runRetconZMQ conn $ enqueueResolveDiff diff_id ops
        result `shouldBe` ()

    it "replies to notify requests" $ do
        let note = ChangeNotification "TestEntity" "TestSource" "item1"
        Right result <- runRetconZMQ conn $ enqueueChangeNotification note
        result `shouldBe` ()

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
    let retconOpt = RetconOptions False LogStdout db Nothing
    retconConfig <- prepareConfig (retconOpt, []) entities

    -- Spawn the server.
    server <- async $ apiServer retconConfig serverConfig

    -- Run the test suite.
    hspec (suite conn)

    -- Shut the server down.
    cancel server