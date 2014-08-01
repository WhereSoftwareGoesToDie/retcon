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
        let d1 = Diff () [InsertOp () ["value"] "one"]
        let d2 = Diff () [InsertOp () ["value"] "two"]

        it "should ignore all changes which conflict" $ do
            let d' = mergeWithPolicy ignoreConflicts d1 d2
            d' `shouldBe` mempty

    describe "rejectAll merge policy" $ do
        let d1 = Diff () [InsertOp () ["value"] "one"]
        let d2 = Diff () [InsertOp () ["value"] "two"]
        
        it "should ignore all changes" $ do
            let d' = mergeWithPolicy rejectAll d1 d2
            d' `shouldBe` mempty

    describe "acceptAll merge policy" $ do
        let d1 = Diff () [InsertOp () ["value"] "one"]
        let d2 = Diff () [InsertOp () ["value"] "two"]
        
        it "should include all changes" $ do
            let d' = mergeWithPolicy acceptAll d1 d2
            d' `shouldBe` Diff () [ InsertOp () ["value"] "one"
                                  , InsertOp () ["value"] "two"
                                  ]

main :: IO ()
main = hspec suite
