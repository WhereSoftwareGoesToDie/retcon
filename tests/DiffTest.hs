{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Text

import Retcon.Diff
import Retcon.Document

import TestHelpers

-- | Proposition: 'emptyDiff' is a unit for 'applyPatch'.
prop_applyDiffUnit :: Document -> Bool
prop_applyDiffUnit doc = applyDiff emptyDiff doc == doc

-- | Proposition: 'applyDiff (diff doc1 doc2) doc1 == doc2'
prop_applyDiff :: Document -> Document -> Bool
prop_applyDiff doc1 doc2 = doc2 == (applyDiff patch doc1)
  where patch = diff doc1 doc2

-- | Proposition: 'applyDiff patch' is idempotent.
prop_applyDiffIdem :: Document -> Document -> Bool
prop_applyDiffIdem doc1 doc2 = doc2 == applyDiff patch (applyDiff patch doc1)
  where patch = diff doc1 doc2

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

    -- prop "patch (diff empty doc) empty == doc"
    -- prop "patch (diff doc empty) doc == empty"
    it "should be idempotent with empty diffs" $ do
      doc <- testLoad' "01-diff-source.json"
      let doc' = applyDiff emptyDiff doc
      doc' `shouldBe` doc

    it "applyDiff (diff empty doc) empty = doc" $ do
      doc <- testLoad' "01-diff-source.json"
      let patch = diff emptyDocument doc
      let doc' = applyDiff patch emptyDocument
      doc' `shouldBe` doc

-- | Run the test suite.
main :: IO ()
main = hspec suite

