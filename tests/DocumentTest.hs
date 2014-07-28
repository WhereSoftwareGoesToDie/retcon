{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson
import Test.Hspec

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

    it "roundtrip should be idempotent" $ do
      let doc' = (decode $ encode testJSON)
      doc' `shouldBe` (Just testDocument)

main :: IO ()
main = hspec suite

