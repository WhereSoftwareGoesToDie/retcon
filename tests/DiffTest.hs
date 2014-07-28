{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Retcon.Diff
import Retcon.Document

import TestHelpers

-- | Hspec test suite for the 'Retcon.Diff' module.
suite :: Spec
suite = do
  describe "generating a diff" $ do
    it "should work with canned documents 01-*.json" $ do
      -- Load the source document.
      source' <- testLoad "01-diff-source.json"
      let source = maybe (error "Couldn't load 01-diff-source.json") id source'
      -- Load the target document.
      target' <- testLoad "01-diff-target.json"
      let target = maybe (error "Couldn't load 01-diff-target.json") id target'
      -- Diff them.
      let patch = diff source target
          doc = applyDiff patch source
      doc `shouldBe` target

  describe "applying a diff" $ do
    it "should be idempotent with empty diffs" $ do
      let doc = emptyDocument
          doc' = applyDiff emptyDiff doc
      doc' `shouldBe` doc

    it "apply (diff empty doc) empty = doc" $ do
      let patch = diff emptyDocument testDocument
          doc = applyDiff patch emptyDocument
      doc `shouldBe` testDocument

-- | Run the test suite.
main :: IO ()
main = hspec suite

