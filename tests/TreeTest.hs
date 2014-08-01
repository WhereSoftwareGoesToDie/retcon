--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Tree.GenericTrie

import TreeHelpers ()

prop_fromListToListId :: Tree Int Int -> Bool
prop_fromListToListId doc = doc == (fromList . toList) doc

-- | Test suite for the edge-labelled tree data structure.
suite :: Spec
suite =
  describe "edge-labelled trees" $
    prop "fromList . toList = id" prop_fromListToListId

-- | Run the test suite.
main :: IO ()
main = hspec suite
