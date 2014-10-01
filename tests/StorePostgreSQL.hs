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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Monad
import Database.PostgreSQL.Simple
import Test.Hspec

import Retcon.Error
import Retcon.Monad
import Retcon.Options
import Retcon.Store
import Retcon.Store.PostgreSQL

options :: RetconOptions
options = defaultOptions

runAction :: PGStorage
          -> RetconMonad RWToken () r
          -> IO (Either RetconError r)
runAction store action = runRetconMonad options [] (token store) () action

postgresqlSuite :: Spec
postgresqlSuite =
    describe "PostgreSQL storage backend" $ do
        it "should be connected when initialised" $ do
            -- postgresql-simple doesn't seem to provide a function similar to
            -- PQstatus from libpq; so let's just try *using* the connection!
            store@(PGStore conn) <- storeInitialise options

            [Only (one :: Int)] <- query_ conn "SELECT 1 + 0;"

            one `shouldBe` 1

        it "should be disconnected when finalised" $ do
            let q conn = query_ conn "SELECT 1 + 0;" :: IO [Only Int]

            store@(PGStore conn) <- storeInitialise options
            [Only one] <- q conn
            one `shouldBe` 1

            storeFinalise store
            (void $ q conn) `shouldThrow` (\e -> sqlErrorMsg e == "connection disconnected")

        it "should allocate and delete internal keys" $ do
            pendingWith "Unimplemented"

        it "should associate foreign and internal keys" $ do
            pendingWith "Unimplemented"

        it "should record initial documents" $ do
            pendingWith "Unimplemented"

        it "should record diffs" $ do
            pendingWith "Unimplemented"

main :: IO ()
main = hspec postgresqlSuite

