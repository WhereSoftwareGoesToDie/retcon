{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import qualified Data.HashMap.Lazy as H
import qualified Data.Map          as M
import           Data.Maybe
import           System.Exit
import           Test.Hspec

import Retcon.Diff
import Retcon.Document

import TestHelpers

-- | A sample 'Diff'.
testDiff :: Diff (Int, String)
testDiff = Diff (1, "hello")
    [ InsertOp (2, "never") ["name"] "Thomas Two"
    , InsertOp (3, "gonna") ["name"] "Thomas Three"
    , InsertOp (4, "give") ["name"] "Thomas Four"
    , InsertOp (5, "you") ["name"] "Thomas Five"
    , InsertOp (6, "up") ["name"] "Thomas Six"
    ]

-- | A sample 'Document'.
testDocument :: Document
testDocument = Document $ M.fromList [("name", Value "Original Thomas")]

-- | Hspec test suite for the 'Retcon.Diff' module.
suite :: Spec
suite = do
  describe "applying a diff" $ do
    it "should be idempotent with empty diffs" $ do
      let doc = emptyDocument
          doc' = applyDiff emptyDiff doc
      doc' `shouldBe` doc

    it "apply (diff empty doc) empty = doc" $ do
      let patch = diff emptyDocument testDocument
          doc = applyDiff patch emptyDocument
      doc `shouldBe` testDocument

  describe "" $ do
    it "" $ do
      -- Try to diff the documents.
      --let diff' = diff source target

      1 `shouldBe` 2

-- | Run the test suite.
main :: IO ()
main = hspec suite

