--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TreeHelpers where

import Control.Applicative
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck

import Data.Tree.GenericTrie

-- | An approximation of the branching factor to be used in randomly generated
-- trees.
--
-- This is currently a damn lie but should probably be a statistic at some point
-- in the future.
branchingFactor :: Int
branchingFactor = 8

-- | Generate a random tree with roughtly the given number of nodes.
--
-- This is thorougly biased, but will definitely terminate. Essentially
-- we generate trees according to the size in the following cases:
--
-- - size 1 - make a single leaf node.
-- - size <= 5 - make a node with leafish children.
-- - size > 5 - split it into roughly branchingFactor branches.
--
-- This will only generate JSON-ish trees.
generateTree :: (Ord key, Arbitrary key, Arbitrary value)
             => Int
             -> Gen (Tree key value)
generateTree 0 = return $ Node Nothing M.empty
generateTree 1 = Node <$> (Just <$> arbitrary) <*> return M.empty
generateTree n = do
    branches <- choose (1, branchingFactor)
    assign <- vectorOf (n-1) $ choose (1, branches)
    kids <- mapM (generateTree . length) $ group $ sort assign
    value <- case kids of
        [] -> arbitrary
        _  -> return Nothing
    kids_labels <- vectorOf (n-1) arbitrary
    return $ Node value $ M.fromList $ zip kids_labels kids

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Tree k v) where
  arbitrary = sized generateTree

