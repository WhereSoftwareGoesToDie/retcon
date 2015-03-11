--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Test.Hspec
import Test.HUnit

import Synchronise

source :: DataSource
source = DataSource
    { sourceEntity = "entity"
    , sourceName = "source"
    , sourceDescription = Nothing
    , commandCreate = "cat > entity/source/%fk.json"
    , commandRead = "cat entity/source/%fk.json"
    , commandUpdate = "cat > entity/source/%fk.json"
    , commandDelete = "rm entity/source/%fk.json"
    }

suite :: Spec
suite = do
    describe "DataSource" $ do
        it "can create and read out again" $ do
            let doc = Document "entity" "source" (object ["foo" .= ("bar" :: String)])
            fk' <- runDSMonad $ createDocument source doc
            fk <- case fk' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            res <- runDSMonad $ readDocument source fk
            case res of
                Left err -> assertFailure (show err)
                Right doc' -> assertBool "Read returned different object than Created" (doc == doc')

main :: IO ()
main = hspec suite
