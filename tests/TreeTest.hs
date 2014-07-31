module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Tree.EdgeLabelled

import TreeHelpers

prop_fromListToListId :: Tree Int Int -> Bool
prop_fromListToListId doc = doc == (fromList . toList) doc

-- | Test suite for the edge-labelled tree data structure.
suite :: Spec
suite = do
  describe "edge-labelled trees" $ do
    prop "fromList . toList = id" prop_fromListToListId

-- | Run the test suite.
main :: IO ()
main = hspec suite
