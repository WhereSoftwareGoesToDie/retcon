--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Command-line application to process individual events.
--
-- This is a sample command-line interface to process events using the retcon
-- algorithm. It is intended as an example and demonstration piece more than a
-- useful tool.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Data.Proxy
import Database.PostgreSQL.Simple
import GHC.TypeLits
import System.Directory
import System.Environment
import System.FilePath

import Retcon.DataSource
import Retcon.DataSource.JsonDirectory
import Retcon.Handler

-- * Entity definitions

instance RetconEntity "customer" where
    entitySources _ = [ SomeDataSource (Proxy :: Proxy "json")
                      , SomeDataSource (Proxy :: Proxy "json2")
                      ]

instance RetconEntity "event" where
    entitySources _ = [ SomeDataSource (Proxy :: Proxy "json")
                      , SomeDataSource (Proxy :: Proxy "icalendar")
                      , SomeDataSource (Proxy :: Proxy "exchange")
                      ]

-- * Data sources

instance RetconDataSource "customer" "json" where
    getDocument key = do
        f <- fp "customer" "json"
        doc <- getJsonDirDocument f key
        either (error . show) return doc

    setDocument doc key = do
        f <- fp "customer" "json"
        key' <- setJsonDirDocument f doc key
        either (error . show) (maybe (error "No key") return) key'

    deleteDocument key = do
        f <- fp "customer" "json"
        res <- deleteJsonDirDocument f key
        either (error . show) return res

instance RetconDataSource "customer" "json2" where
    getDocument key = do
        f <- fp "customer" "json2"
        doc <- getJsonDirDocument f key
        either (error . show) return doc

    setDocument doc key = do
        f <- fp "customer" "json2"
        key' <- setJsonDirDocument f doc key
        either (error . show) (maybe (error "No key") return) key'

    deleteDocument key = do
        f <- fp "customer" "json2"
        res <- deleteJsonDirDocument f key
        either (error . show) return res

instance RetconDataSource "event" "json" where
    getDocument key = do
        f <- fp "event" "json"
        doc <- getJsonDirDocument f key
        either (error . show) return doc

    setDocument doc key = do
        f <- fp "event" "json"
        key' <- setJsonDirDocument f doc key
        either (error . show) (maybe (error "No key") return) key'

    deleteDocument key = do
        f <- fp "event" "json"
        res <- deleteJsonDirDocument f key
        either (error . show) return res

instance RetconDataSource "event" "icalendar" where
    getDocument key = do
        f <- fp "event" "icalendar"
        doc <- getJsonDirDocument f key
        either (error . show) return doc

    setDocument doc key = do
        f <- fp "event" "icalendar"
        key' <- setJsonDirDocument f doc key
        either (error . show) (maybe (error "No key") return) key'

    deleteDocument key = do
        f <- fp "event" "icalendar"
        res <- deleteJsonDirDocument f key
        either (error . show) return res

instance RetconDataSource "event" "exchange" where
    getDocument key = do
        f <- fp "event" "exchange"
        doc <- getJsonDirDocument f key
        either (error . show) return doc

    setDocument doc key = do
        f <- fp "event" "exchange"
        key' <- setJsonDirDocument f doc key
        either (error . show) (maybe (error "No key") return) key'

    deleteDocument key = do
        f <- fp "event" "exchange"
        res <- deleteJsonDirDocument f key
        either (error . show) return res

-- | Make file path
fp :: String -> String -> IO FilePath
fp source entity = do
    cwd <- getCurrentDirectory
    return $ joinPath [cwd, "tests", "data", source, entity]

-- * Retcon configuration

cfg :: RetconConfig
cfg = RetconConfig [ SomeEntity (Proxy :: Proxy "event")
                   , SomeEntity (Proxy :: Proxy "customer")
                   ]

-- * Parse and execute commands

-- $ TODO: add simple configuration handling.

-- | Parse event from command line and execute it.
main :: IO ()
main = do
    (entity:source:key:_) <- getArgs
    conn <- connectPostgreSQL "dbname='retcon'"
    (res, msg) <- retcon cfg conn $ show (entity, source, key)
    print msg
    return $ either (const ()) (id) res

