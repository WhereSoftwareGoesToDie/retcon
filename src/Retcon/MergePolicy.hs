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

module Retcon.MergePolicy where

import Data.List
import GHC.TypeLits

import Retcon.Document
import Retcon.Diff

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
                , mergeDiffs :: Diff l -> Diff l -> (Diff l, Diff l)
                -- ^ Combine two diffs. The values are acc, new, (acc', new').
                }

-- * Using policies

-- | Merge two diffs by applying a 'MergePolicy'.
mergeWithPolicy :: MergePolicy l -> Diff l -> Diff l -> Diff l
mergeWithPolicy policy d1 d2 = fst $ (mergeDiffs policy) d1 d2

-- * Basic policies

-- $ There are a small number of basic policies which can be used to describe
-- many more complex policies. 

-- | Policy: reject all changes.
rejectAll :: MergePolicy ()
rejectAll = MergePolicy (const ()) merge 
  where
    merge (Diff ll opl) (Diff lr opr) = (Diff ll [], Diff lr (opl ++ opr))

-- | Policy: accept all changes.
--
-- All changes will be applied in whatever arbitrary order they are encountered.
acceptAll :: MergePolicy ()
acceptAll = MergePolicy (const ()) merge
  where
    merge (Diff ll opl) (Diff lr opr) = (Diff ll (opl ++ opr), Diff lr [])

-- | Policy: reject all conflicting changes.
ignoreConflicts :: MergePolicy ()
ignoreConflicts = MergePolicy (const ()) merge
  where
    merge (Diff ll opl) (Diff lr opr) =
        let keysl = nub $ map diffOpTarget opl
            keysr = nub $ map diffOpTarget opr
            keyconflicts = (intersect keysl keysr)
            changes = opl ++ opr
            clash = filter (flip diffOpAffects keyconflicts) $ changes
            noclash = filter (not . flip diffOpAffects keyconflicts) $ changes
        in (Diff ll noclash, Diff lr clash)

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
combine (MergePolicy el ml) (MergePolicy er mr) = MergePolicy extract merge
  where
    extract doc = (el doc, er doc)
    merge d1 d2 = error "combine: not implemented"

test1 =
  onField ["payment", "status"] $ trustSource (someSymbolVal "accounts") `combine`
  ignoreConflicts

