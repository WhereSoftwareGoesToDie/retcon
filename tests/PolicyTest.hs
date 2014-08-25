--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Test.Hspec

import Retcon.Diff
import Retcon.MergePolicy

suite :: Spec
suite = do
    describe "ignoreConflicts merge policy" $ do
        let d1 = Diff () [ InsertOp () ["value"] "one"
                         , InsertOp () ["pet"] "cat"]
        let d2 = Diff () [ InsertOp () ["value"] "two"
                         , DeleteOp () ["food"]]

        it "should ignore all changes which conflict" $ do
            let d' = mergeWithPolicy ignoreConflicts [d1,d2]
            d' `shouldBe` Diff () [ InsertOp () ["pet"] "cat"
                                  , DeleteOp () ["food"]]

    describe "rejectAll merge policy" $ do
        let d1 = Diff () [InsertOp () ["value"] "one"]
        let d2 = Diff () [InsertOp () ["value"] "two"]

        it "should ignore all changes" $ do
            let d' = mergeWithPolicy rejectAll [d1,d2]
            d' `shouldBe` mempty

    describe "acceptAll merge policy" $ do
        let d1 = Diff () [InsertOp () ["value"] "one"]
        let d2 = Diff () [InsertOp () ["value"] "two"]

        it "should include all changes" $ do
            let d' = mergeWithPolicy acceptAll [d1,d2]
            d' `shouldBe` Diff () [ InsertOp () ["value"] "one"
                                  , InsertOp () ["value"] "two"
                                  ]

brokenSuite :: Spec
brokenSuite = do
    describe "acceptAll <+> onField value rejectAll" $ do
        let policy = acceptAll `combine` onField ["value"] rejectAll

        let label = extractLabel policy mempty
        let d1 = Diff label [ InsertOp label ["value"] "one"
                            , InsertOp label ["cats"] "ceiling"]
        let d2 = Diff label [ InsertOp label ["value"] "two"
                            , InsertOp label ["cats"] "long"]
        let d3 = Diff label [ InsertOp label ["favourite"] "giraffe"
                            , InsertOp label ["dogs"] "no"]

        it "should ignore single change to value" $ do
            let d' = mergeWithPolicy policy [d1,d3]
            d' `shouldBe` Diff label [ InsertOp label ["cats"] "ceiling"
                                     , InsertOp label ["favourite"] "giraffe"
                                     , InsertOp label ["dogs"] "no"
                                     ]

        it "should ignore multiple changes to value" $ do
            let d' = mergeWithPolicy policy [d1,d2]
            d' `shouldBe` Diff label [ InsertOp label ["cats"] "ceiling"
                                     , InsertOp label ["cats"] "long"
                                     ]

        it "should leave other changes alone" $ do
            1 `shouldBe` 2

main :: IO ()
main = hspec suite
