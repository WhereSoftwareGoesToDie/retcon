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

import Data.Aeson.Diff (Operation(..))
import qualified Data.Aeson.Diff as D
import Test.Hspec

import Synchronise.Diff

suite :: Spec
suite = do
    describe "ignoreConflicts merge policy" $ do
        let d1 = mkPatch [ Ins [D.OKey "value"] "one"
                         , Ins [D.OKey "pet"] "cat"]
        let d2 = mkPatch [ Ins [D.OKey "value"] "two"
                         , Del [D.OKey "food"] ""]

        it "should ignore all changes which conflict" $ do
            let (d',c) = merge ignoreConflicts d1 d2
            d' `shouldBe` mkPatch [ Ins [D.OKey "pet"] "cat"
                                  , Del [D.OKey "food"] ""]
            c  `shouldBe` map mkRej [ Ins [D.OKey "value"] "one"
                                    , Ins [D.OKey "value"] "two" ]

    describe "rejectAll merge policy" $ do
        let d1 = mkPatch [Ins [D.OKey "value"] "one"]
        let d2 = mkPatch [Ins [D.OKey "value"] "two"]

        it "should ignore all changes" $ do
            let (d',_) = merge rejectAll d1 d2
            d' `shouldBe` emptyPatch

    describe "acceptAll merge policy" $ do
        let d1 = mkPatch [Ins [D.OKey "value"] "one"]
        let d2 = mkPatch [Ins [D.OKey "value"] "two"]

        it "should include all changes" $ do
            let (d',_) = merge acceptAll d1 d2
            d' `shouldBe` mkPatch [ Ins [D.OKey "value"] "one"
                                  , Ins [D.OKey "value"] "two"
                                  ]

mkPatch = Patch Unamed . D.Patch
mkRej   = RejectedOp Unamed

main :: IO ()
main = hspec suite
