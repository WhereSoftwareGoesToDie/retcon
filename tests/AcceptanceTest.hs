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
import Data.Proxy

import Retcon.DataSource
import Retcon.DataSource.JsonDirectory
import Retcon.Monad

import Test.Hspec
import TestHelpers

-- * Entity definitions

instance RetconEntity "entity" where
    entitySources _ = [ SomeDataSource (Proxy :: Proxy "upstream")
                      , SomeDataSource (Proxy :: Proxy "downstream")
                      ]

-- * Data sources

instance RetconDataSource "entity" "upstream" where

    data DataSourceState "entity" "upstream" = Upstream FilePath

    initialiseState = Upstream <$> testJSONFilePath
    finaliseState _ = return ()

    getDocument key =
        getActionState >>= \(Upstream fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(Upstream fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(Upstream fp) -> deleteJSONDir fp key


instance RetconDataSource "entity" "downstream" where

    data DataSourceState "entity" "downstream" = Downstream FilePath
    initialiseState = Downstream <$> testJSONFilePath
    finaliseState _ = return ()

    getDocument key =
        getActionState >>= \(Downstream fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(Downstream fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(Downstream fp) -> deleteJSONDir fp key

-- * Retcon configuration
cfg :: RetconConfig
cfg = RetconConfig [ SomeEntity (Proxy :: Proxy "entity") ]

main :: IO ()
main = hspec suite

suite :: Spec
suite =
    describe "Upstream change is propogated locally" $
        it "todo" $ pending 
