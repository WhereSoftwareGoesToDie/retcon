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

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Control.Lens.Operators
import Control.Monad
import Data.Proxy
import Database.PostgreSQL.Simple

import Retcon.DataSource
import Retcon.DataSource.JsonDirectory
import Retcon.Handler
import Retcon.Monad
import Retcon.Options
import Retcon.Store (token)
import Retcon.Store.PostgreSQL
import TestHelpers

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

    data DataSourceState "customer" "json" = CustPath FilePath

    initialiseState = CustPath <$> testJSONFilePath
    finaliseState _ = return ()

    getDocument key =
        getActionState >>= \(CustPath fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(CustPath fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(CustPath fp) -> deleteJSONDir fp key

instance RetconDataSource "customer" "json2" where

    data DataSourceState "customer" "json2" = CustPath2 FilePath

    initialiseState = CustPath2 <$> testJSONFilePath
    finaliseState _ = return ()


    getDocument key =
        getActionState >>= \(CustPath2 fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(CustPath2 fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(CustPath2 fp) -> deleteJSONDir fp key

instance RetconDataSource "event" "json" where

    data DataSourceState "event" "json" = JSONEventPath FilePath

    initialiseState = JSONEventPath <$> testJSONFilePath
    finaliseState _ = return ()

    getDocument key =
        getActionState >>= \(JSONEventPath fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(JSONEventPath fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(JSONEventPath fp) -> deleteJSONDir fp key

instance RetconDataSource "event" "icalendar" where

    data DataSourceState "event" "icalendar" = CalPath FilePath

    initialiseState = CalPath <$> testJSONFilePath
    finaliseState _ = return ()

    getDocument key =
        getActionState >>= \(CalPath fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(CalPath fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(CalPath fp) -> deleteJSONDir fp key

instance RetconDataSource "event" "exchange" where

    data DataSourceState "event" "exchange" = ExchangePath FilePath

    initialiseState = ExchangePath <$> testJSONFilePath
    finaliseState _ = return ()

    getDocument key =
        getActionState >>= \(ExchangePath fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(ExchangePath fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(ExchangePath fp) -> deleteJSONDir fp key

-- * Retcon configuration

cfg :: RetconConfig
cfg = RetconConfig [ SomeEntity (Proxy :: Proxy "event")
                   , SomeEntity (Proxy :: Proxy "customer")
                   ]

-- * Parse and execute commands

-- | Parse event from command line and execute it.
main :: IO ()
main = do
    opts <- parseArgsWithConfig "/etc/retcon.conf"
    when (opts ^. optVerbose) $ print opts

    tok <- token . PGStore <$> connectPostgreSQL (opts ^. optDB)
    let (entity:source:key:_) = opts ^. optArgs
    res <- retcon opts cfg tok $ show (entity, source, key)
    print res
