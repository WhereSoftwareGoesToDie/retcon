--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Description: Diff and patch /Synchronise/ documents.
module Synchronise.Diff
     ( -- * Operations
       diff
     , patch
     , mergePatches

       -- * Merge policies
     , MergePolicy(..)
     , acceptAll
     , rejectAll
     , ignoreConflicts
     , trustOnlySource

       -- * Representing patches
     , Patch(..)
     , patchLabel
     , patchDiff
     , RejectedOp(..)
     , rejectedLabel
     , rejectedOperation
     ) where

import Control.Lens hiding ((.=))
import qualified Data.Aeson.Diff as D
import Data.Monoid

import Synchronise.Document
import Synchronise.Identifier

-- | A /synchronised/ 'Patch' is an @aeson-diff@ patch together with a label.
data Patch l = Patch
    { _patchLabel :: l
    , _patchDiff  :: D.Patch
    }
  deriving (Eq, Show)
makeLenses ''Patch

-- | An @aeson-diff@ patch operation which was excluded be a 'MergePolicy'.
data RejectedOp l = RejectedOp
    { _rejectedLabel     :: l
    , _rejectedOperation :: D.Operation
    }
  deriving (Eq, Show)
makeLenses ''RejectedOp

-- | Describes the way we merge diffs.
data MergePolicy l = MergePolicy
    { extractLabel :: Document -> l
    , mergePatch   :: Patch l -> Patch l -> (Patch l, [RejectedOp l])
    }

-- | Accept all changes, and let them stomp all over each other.
acceptAll :: MergePolicy ()
acceptAll = MergePolicy{..}
  where
    extractLabel = const ()
    mergePatch p1 p2 =
        let p = Patch () $ (p1 ^. patchDiff) <> (p2 ^. patchDiff)
            r = []
        in (p,r)

-- | Reject all changes.
rejectAll :: MergePolicy ()
rejectAll = MergePolicy{..}
  where
    extractLabel = const ()
    mergePatch p1 p2 =
        let p = Patch () mempty
            r1 = reject p1
            r2 = reject p2
        in (p, r1 <> r2)

-- | Trust only patches from a particular 'DataSource', discarding all other
-- changes.
trustOnlySource :: SourceName -> MergePolicy SourceName
trustOnlySource name = MergePolicy {..}
  where
    extractLabel d = d ^. documentSource
    mergePatch p1 p2
        | name == (p2 ^. patchLabel) = (p2, reject p1)
        | name == (p1 ^. patchLabel) = (p1, reject p2)
        | otherwise =
            let
                p = Patch name mempty
                r = reject p1 <> reject p2
            in (p,r)

-- | Merge diffs, ignoring all changes which conflict.
--
-- TODO(thsutton) Implement logic here.
ignoreConflicts :: MergePolicy ()
ignoreConflicts = MergePolicy{..}
  where
    extractLabel = const ()
    mergePatch p1 p2 =
        let p = Patch () mempty
            r = reject p1 <> reject p2
        in (p,r)

-- | Use a 'MergePolicy' to compare two versions of a document and extract a
-- 'Patch' describing changes.
diff
    :: MergePolicy l
    -> Document
    -> Document
    -> Patch l
diff MergePolicy{..} d1 d2 =
    let j1 = d1 ^. documentContent
        j2 = d2 ^. documentContent
        l = extractLabel d2
        d = D.diff j1 j2
    in Patch l d

-- | Use a 'MergePolicy' to apply a 'Patch' to a 'Document'.
patch
    :: MergePolicy l
    -> Patch l
    -> Document
    -> Document
patch MergePolicy{} p d = d & documentContent %~ D.patch (p ^. patchDiff)

-- | Combine two 'Patch'es according to the rules of a 'MergePolicy'.
--
-- This allows case-specific criteria to be used in resolving ambiguities which
-- might arise when resolving conflicts between patches.
--
-- TODO(thsutton) Rename the record field and delete this?
mergePatches
    :: Monoid l
    => MergePolicy l
    -> Patch l
    -> Patch l
    -> (Patch l, [RejectedOp l])
mergePatches MergePolicy{..} = mergePatch

-- * Utility

-- | Convert all the 'D.Operation's in a 'D.Patch' into 'RejectedOp's.
reject :: Patch l -> [RejectedOp l]
reject p = fmap (RejectedOp (p ^. patchLabel)) . D.patchOperations $ p ^. patchDiff
