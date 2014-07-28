{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Retcon.Diff
import Retcon.Document

import TestHelpers

-- | Hspec test suite for the 'Retcon.Diff' module.
suite :: Spec
suite = do
  describe "key value pairs" $ do
    it "should sort by key" $ do
      let keys = map fst $ toKVList testDocument
      keys `shouldBe` [ ["address", "company"]
                      , ["address", "locality"]
                      , ["address", "street"]
                      , ["age"]
                      , ["name"]
                      ]

    it "should handle values" $ do
      let kvs = drop 3 $ toKVList testDocument
      kvs `shouldBe` [ (["age"], "30")
                     , (["name"], "Thomas Sutton")
                     ]

    it "should handle subdocuments" $ do
      let kvs = take 3 $ toKVList testDocument
      kvs `shouldBe` [ (["address", "company"], "Anchor")
                     , (["address", "locality"], "Sydney")
                     , (["address", "street"], "Level 11 / 201 Elizabeth Street")
                     ]

  describe "generating a diff" $ do
    it "should work with canned documents 01-*.json" $ do
      source <- testLoad' "01-diff-source.json"
      target <- testLoad' "01-diff-target.json"
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

