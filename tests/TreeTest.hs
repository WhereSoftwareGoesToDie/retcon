module Main where

import Test.Hspec

import Data.Tree.EdgeLabelled

-- | Test suite for the edge-labelled tree data structure.
suite :: Spec
suite = do
  describe "edge-labelled trees" $ do
    it "fromList" $ do
      1 `shouldBe` 2

    it "toList" $ do
      1 `shouldBe` 2

    it "fromList . toList = id" $ do
      1 `shouldBe` 2

-- | Run the test suite.
main :: IO ()
main = hspec suite
