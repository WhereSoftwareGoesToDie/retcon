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

import Data.Maybe
import Data.Monoid
import Test.Hspec

import Retcon.Diff
import Retcon.Document

import TestHelpers

suite :: Spec
suite = do
  describe "JSON marhsalling" $ do
    it "can load 01-diff-source.json" $ do
      test1 <- testLoad "01-diff-source.json"
      maybe (error "Could not load file") (const pass) test1

    it "can load 01-diff-target.json" $ do
      test2 <- testLoad "01-diff-target.json"
      maybe (error "Could not load file") (const pass) test2

  describe "initial document calculation" $ do
    it "should be empty whenever the input list is empty" $ do
      calculateInitialDocument [] `shouldBe` mempty

    it "should be empty whenever an empty is input" $ do
      test1 <- testLoad "01-diff-source.json"
      test2 <- testLoad "01-diff-target.json"
      let documents = mempty:catMaybes [test1, test2]
      calculateInitialDocument documents `shouldBe` mempty

    it "should result in diffs with no deletions from the inputs" $ do
      test1 <- testLoad "01-diff-source.json"
      test2 <- testLoad "01-diff-target.json"
      let documents = catMaybes [test1, test2]
      let initial = calculateInitialDocument documents
      let diffs = concatMap (diffChanges . diff initial) documents
      let deletions = filter diffOpIsDelete diffs
      length deletions `shouldBe` 0

main :: IO ()
main = hspec suite

