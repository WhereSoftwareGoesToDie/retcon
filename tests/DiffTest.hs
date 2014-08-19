--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Data.Monoid
import Data.Aeson
import Data.Text
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Retcon.Diff
import Retcon.Document
import TestHelpers

instance Arbitrary Document where
    arbitrary = fmap Document arbitrary

instance Arbitrary (Diff Text) where
    arbitrary = Diff <$> arbitrary <*> arbitrary

instance Arbitrary (DiffOp Text) where
    arbitrary = oneof [
                InsertOp <$> arbitrary <*> arbitrary <*> arbitrary,
                DeleteOp <$> arbitrary <*> arbitrary
                ]

-- | Proposition: 'mempty' is a unit for 'applyPatch'.
prop_applyDiffUnit :: Document -> Bool
prop_applyDiffUnit doc = applyDiff (mempty :: Diff ()) doc == doc

-- | Proposition: 'applyDiff (diff doc1 doc2) doc1 == doc2'
prop_applyDiff :: Document -> Document -> Bool
prop_applyDiff doc1 doc2 = doc2 == applyDiff patch doc1
  where patch = diff doc1 doc2

-- | Proposition: 'applyDiff patch' is idempotent.
prop_applyDiffIdem :: Document -> Document -> Bool
prop_applyDiffIdem doc1 doc2 = doc2 == applyDiff patch (applyDiff patch doc1)
  where patch = diff doc1 doc2

-- | Proposition: Diff objects can be converted into JSON form and back again.
prop_diffJsonSerialisable :: Diff Text -> Bool
prop_diffJsonSerialisable diff = (decode $ encode diff) == (Just diff)

-- | Proposition: DiffOp objects can be converted into JSON form and back again.
prop_diffopJsonSerialisable :: DiffOp Text -> Bool
prop_diffopJsonSerialisable diffop = (decode $ encode diffop) == (Just diffop)

-- | Hspec test suite for the 'Retcon.Diff' module.
suite :: Spec
suite = do
  describe "generating a diff" $ do
    it "should work with canned documents 01-*.json" $ do
      source <- testLoad' "01-diff-source.json"
      target <- testLoad' "01-diff-target.json"
      let patch = diff source target
          doc = applyDiff patch source
      doc `shouldBe` target

  describe "applying a diff" $ do
    prop "applyDiff" prop_applyDiff

    prop "applyDiff (unit)" prop_applyDiffUnit

    prop "applyDiff (idem)" prop_applyDiffIdem

  describe "serialising a diff" $ do
    prop "serialiseJSON (Diff)" prop_diffJsonSerialisable

  describe "serialising a diff operation" $ do
    prop "serialiseJSON (DiffOp)" prop_diffopJsonSerialisable

-- | Run the test suite.
main :: IO ()
main = hspec suite

