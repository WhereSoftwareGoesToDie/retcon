--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Policy to merge diffs from different sources.
--
-- This module implements 'MergePolicy's which control the integration of
-- changes from different sources.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Retcon.MergePolicy where

import Control.Lens
import Data.List

import Retcon.Diff

-- * Merge policies

-- $ A merge policy allows the system to combine diffs according to some
-- requirements (hopefully some sort of business rules). Each 'MergePolicy'
-- contains the following elements:
--
-- - A function to use, using that information, combine two 'Diff's.

-- | A policy to merge 'Diff's using some arbitrary information @l@.
data MergePolicy l = MergePolicy
    { mergeDiffs   :: [Diff l] -> (Diff l, [Diff l])
    -- ^ Combine diffs, returning a merged diff and left-over fragments.
    }

-- * Using policies

-- | Merge two diffs by applying a 'MergePolicy'.
mergeWithPolicy :: MergePolicy l -> [Diff l] -> Diff l
mergeWithPolicy policy ds = fst $ mergeDiffs policy ds

-- * Basic policies

-- $ There are a small number of basic policies which can be used to describe
-- many more complex policies.

-- | Policy: reject all changes.
--
-- All input 'Diff's are rejected and the merged 'Diff' is empty.
rejectAll :: MergePolicy ()
rejectAll = MergePolicy (Diff () [],)

-- | Policy: accept all changes.
--
-- All changes will be applied in whatever arbitrary order they are encountered.
acceptAll :: MergePolicy ()
acceptAll = MergePolicy
    (\ds -> (Diff () (concatOf (traversed . diffChanges) ds), []))

-- | Policy: reject all conflicting changes.
--
-- Changes will be applied iff they are the sole change affecting that key; all
-- other changes will be rejected.
ignoreConflicts :: MergePolicy ()
ignoreConflicts = MergePolicy merge
  where
    merge ds =
        let ops = concatOf (traversed . diffChanges) ds
            keys = group . sort . map diffOpTarget $ ops
            keyconflicts = map head . filter (\l -> length l > 1) $ keys
            noclash = filter (not . (`diffOpAffects` keyconflicts)) ops
            scraps = ds & traversed . diffChanges %~ filter (`diffOpAffects` keyconflicts)
        in (Diff () noclash, scraps)
