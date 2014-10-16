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
import GHC.TypeLits

import Retcon.Diff
import Retcon.Document

-- * Merge policies

-- $ A merge policy allows the system to combine diffs according to some
-- requirements (hopefully some sort of business rules). Many policies will
-- require some metadata about changes to make their decisions, so a merge
-- policy is comprised of several elements:
--
-- - A function to extract the particular metadata required the policy; and
--
-- - A function to use, using that information, combine two 'Diff's.

-- | A policy to merge 'Diff's using some arbitrary information @l@.
data MergePolicy l =
    MergePolicy { extractLabel :: Document -> l
                -- ^ Extract the label information needed to apply this policy.
                , mergeDiffs   :: [Diff l] -> (Diff l, [Diff l])
                -- ^ Combine a collection of diffs; returning a merged diff and left-over fragments.
                }

-- * Using policies

-- | Merge two diffs by applying a 'MergePolicy'.
mergeWithPolicy :: MergePolicy l -> [Diff l] -> Diff l
mergeWithPolicy policy ds = fst $ mergeDiffs policy ds

-- * Basic policies

-- $ There are a small number of basic policies which can be used to describe
-- many more complex policies.

-- | Policy: reject all changes.
rejectAll :: MergePolicy ()
rejectAll = MergePolicy (const ()) (Diff () [],)

-- | Policy: accept all changes.
--
-- All changes will be applied in whatever arbitrary order they are encountered.
acceptAll :: MergePolicy ()
acceptAll = MergePolicy (const ())
                        (\ds -> (Diff () (concatOf (traversed . diffChanges) ds), []))

-- | Policy: reject all conflicting changes.
ignoreConflicts :: MergePolicy ()
ignoreConflicts = MergePolicy (const ()) merge
  where
    merge ds =
        let ops = concatOf (traversed . diffChanges) ds
            keys = group . sort . map diffOpTarget $ ops
            keyconflicts = map head . filter (\l -> length l > 1) $ keys
            noclash = filter (not . (`diffOpAffects` keyconflicts)) ops
            scraps = ds & traversed . diffChanges %~ filter (`diffOpAffects` keyconflicts)
        in (Diff () noclash, scraps)

-- | TODO: sources should *not* be identified by their strings.
type Source = SomeSymbol

-- | Policy: accept *only* changes from a specific source.
trustSource :: Source -> MergePolicy Source
trustSource source = MergePolicy (const source) (error "trustSource: not implemented")

-- | TODO: we should probably call this "highwater"?
type Timestamp = String

-- | Policy: resolve conflicts in favour of the most recent update.
mostRecent :: MergePolicy Timestamp
mostRecent = MergePolicy (const "") (error "mostRecent: not implemented")

-- * Policy combinators

-- $ Policy combinators can be used to compose existing policies to express
-- more complex requirements.

-- | TODO: a field is the sequence of keys which identify a node.
type Field = [DocumentKey]

-- | Restrict a policy so that it is applied on a specific field.
onField :: Field -> MergePolicy l -> MergePolicy l
onField _field policy = policy

-- | Combine two policies.
combine :: MergePolicy l -> MergePolicy r -> MergePolicy (l,r)
combine (MergePolicy el _) (MergePolicy er _) = MergePolicy extract merge
  where
    extract doc = (el doc, er doc)
    merge _ = error "combine: not implemented"

