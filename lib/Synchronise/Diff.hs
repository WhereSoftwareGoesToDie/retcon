--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor   #-}

-- | Description: Diff and patch /Synchronise/ documents.
module Synchronise.Diff where

import Control.Lens
import Data.Aeson.Diff as D
import Data.Monoid

import Synchronise.Document


data LabelledOp label = LabelledOp
  { opLabel :: label
  , op      :: D.Operation }


--------------------------------------------------------------------------------

-- | A 'D.Diff' with metadata.
data LabelledPatch l = LabelledPatch
    { diffLabel :: l
    , diffDiff  :: D.Patch
    } deriving (Eq, Functor)

instance Monoid l => Monoid (LabelledPatch l) where
    mempty = LabelledPatch mempty mempty

    (LabelledPatch l1 p1) `mappend` (LabelledPatch l2 p2) = LabelledPatch
        (l1 <> l2) (p1 <> p2)

data MergePolicy l = MergePolicy
    { extractLabel :: Document -> l
    , mergePatchs  :: LabelledPatch l -> LabelledPatch l -> LabelledPatch l
    }

diff
    :: MergePolicy l
    -> Document
    -> Document
    -> LabelledPatch l
diff MergePolicy{..} d1 d2 =
    let j1 = d1 ^. documentContent
        j2 = d2 ^. documentContent
        l = extractLabel d2
        d = D.diff j1 j2
    in LabelledPatch l d

-- | Combine two 'LabelledPatch'es according to the rules of a 'MergePolicy'.
--
-- This allows case-specific criteria to be used in resolving ambiguities which
-- might arise when resolving conflicts between patches.
mergePatches
    :: Monoid l
    => MergePolicy l
    -> LabelledPatch l
    -> LabelledPatch l
    -> (LabelledPatch l, (LabelledPatch l, LabelledPatch l))
mergePatches MergePolicy{..} p1 p2 = (mempty, (p1, p2))
