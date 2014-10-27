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
import Control.Monad
import Data.ByteString ()
import qualified Data.ByteString.Char8 as BS
import Data.String
import System.Process
import Test.Hspec

import DBHelpers
import Retcon.DataSource.PostgreSQL
import Retcon.Network.Client
import Retcon.Network.Server
import Retcon.Options
import Retcon.Store.PostgreSQL

prepareDatabase :: DBName -> IO () -> IO ()
prepareDatabase dbname =
    bracket_ setup teardown
  where
    setup =
        resetTestDBWithFixture dbname "retcon.sql"
    teardown =
        void . system . concat $
            [ "dropdb --if-exists ", unDBName dbname, " >/dev/null 2>&1 " ]

suite :: String -> DBName -> Spec
suite conn dbname =
    around (prepareDatabase dbname) . describe "Retcon API" $ do
        -- | A modification is made upstream, Retcon is notified and it makes
        -- the appropriate change locally
        it "upstream change propogates locally" pending

        -- | A record is removed upstream, retcon is notified, identifies this
        -- as a a delete and removes the appropriate record locally.
        it "upstream delete propogates locally" pending

        -- | A record is modified both upstream and downstream in an
        -- incompatible way. Retcon is notified of the upstream and downstream
        -- change (two notifications in total) and correctly indicates a conflict
        -- once.
        --
        -- The local change is chosen and this preference is propogated
        -- upstream.
        it "comparing and resolving diff works" pending

main :: IO ()
main = do
    let conn = "tcp://127.0.0.1:1234"
    let db = DBName "retcon_test"
    let entities = []

    -- Prepare the retcon and server configurations.
    let serverConfig = ServerConfig conn
    let retconOpt = RetconOptions False LogStderr (fromString . unDBName $ db) Nothing
    retconConfig <- prepareConfig (retconOpt, []) entities

    -- Spawn the server.
    server <- async $ apiServer retconConfig serverConfig

    -- Run the test suite.
    hspec (suite conn db)

    -- Shut the server down.
    cancel server
