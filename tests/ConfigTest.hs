-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import qualified Data.Map.Strict as M
import Test.Hspec
import Text.Trifecta

import Retcon.DataSource
import Retcon.Options
import Utility.Configuration

testfile :: FilePath
testfile = "tests/data/parameters.conf"

configurationSuite :: Spec
configurationSuite = do
    describe "Configuration loading" $ do
        it "should parse data" $ do
            params <- parseFromFile configParser testfile
            params `shouldBe` Just
                [ ("dispatchtest", "dispatch1", "uri", "https://api.com/")
                , ("dispatchtest", "dispatch1", "password", "p4ssw0rd")
                , ("dispatchtest", "dispatch2", "server", "db1.example.com")
                , ("dispatchtest", "dispatch2", "database", "databass")
                ]

        it "should transform parsed data" $ do
            params <- (fmap convertConfig) <$> parseFromFile configParser testfile
            params `shouldBe` (Just $ M.fromList
                [ (("dispatchtest", "dispatch1"), M.fromList
                    [ ("uri", "https://api.com/")
                    , ("password", "p4ssw0rd")
                    ])
                , (("dispatchtest", "dispatch2"), M.fromList
                    [ ("server", "db1.example.com")
                    , ("database", "databass")
                    ])
                ])

    describe "Configuration passing" $ do
        it "should lookup data for data source" $ do
            Just params <- (fmap convertConfig) <$> parseFromFile configParser testfile
            pendingWith "Not implemented"

        it "should pass data when initialising a data source" $ do
            pendingWith "Not implemented"

        it "should pass data when finalising a data source" $ do
            pendingWith "Not implemented"

main :: IO ()
main = hspec configurationSuite
