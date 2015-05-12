{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Description: Diff and patch /Retcon/ documents.
module Retcon.Diff
     ( -- * Operations
       diff
     , patch
     , mergeWith
     , extractLabel
     , merge

       -- * Merge policies
     , MergePolicy
     , doNothing
     , acceptAll
     , rejectAll
     , ignoreConflicts
     , trustOnlySource

       -- * Representing patches
     , PatchLabel(..)
     , Patch(..)
     , patchLabel
     , patchDiff
     , emptyPatch
     , RejectedOp(..)
     , rejectedLabel
     , rejectedOperation
     ) where

import           Debug.Trace

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import qualified Data.Aeson.Diff     as D
import           Data.Either
import qualified Data.Map            as M
import           Data.Monoid

import           Retcon.Document
import           Retcon.Identifier

data PatchLabel
  = Unamed           -- ^ ()
  | Name SourceName  -- ^ Named after its origin.
  deriving (Eq, Show)

data Patch = Patch
  { _patchLabel :: PatchLabel
  , _patchDiff  :: D.Patch
  } deriving (Eq, Show)
makeLenses ''Patch

data RejectedOp = RejectedOp
  { _rejectedLabel     :: PatchLabel
  , _rejectedOperation :: D.Operation
  } deriving (Eq, Show)
makeLenses ''RejectedOp

-- | Describes the way we merge diffs.
--
data MergePolicy = MergePolicy
  { extractLabel :: Document -> PatchLabel
  , mergeWith    :: [Patch]  -> MergeResult
  }

type MergeResult = (Patch, [RejectedOp])

emptyPatch :: Patch
emptyPatch = Patch Unamed mempty

--------------------------------------------------------------------------------

-- | Policy: do nothing.
--
--   Be a lazy bum.
doNothing :: MergePolicy
doNothing = MergePolicy {..}
  where
    extractLabel    = const Unamed
    mergeWith ps    = (emptyPatch, concatMap rej ps)
    rej (Patch n o) = map (RejectedOp n) (o ^. patchOperations)

-- | Policy: accept all changes.
--
--   All changes will be applied in whatever arbitrary order they are encountered.
acceptAll :: MergePolicy
acceptAll = MergePolicy {..}
  where
    extractLabel = const Unamed
    mergeWith ps =
        let os = concatMap (^. patchDiff . patchOperations) ps
        in (Patch Unamed (D.Patch os), mempty)

-- | Policy: reject all changes.
--
--   All input are rejected and the merged diff is empty.
rejectAll :: MergePolicy
rejectAll = MergePolicy {..}
  where
    extractLabel = const Unamed
    mergeWith ps = (Patch Unamed mempty, concatMap reject ps)

-- | Trust only patches from a particular 'DataSource', discarding all other
--   changes.
trustOnlySource :: SourceName -> MergePolicy
trustOnlySource n@(Name -> name) = MergePolicy {..}
  where
    extractLabel d = Name $ d ^. documentSource
    mergeWith ps =
        let (acc, rej) = partitionEithers . map hasName $ concatMap reject ps
        in (Patch (Name n) $ D.Patch (map (^. rejectedOperation) acc), rej)
    hasName o = if name == o ^. rejectedLabel then Left o else Right o

-- | Policy: reject all conflicting changes.
--
--   Changes will be applied iff they are the sole change affecting that key; all
--   other changes will be rejected.
ignoreConflicts :: MergePolicy
ignoreConflicts = MergePolicy {..}
  where
    extractLabel = const Unamed
    mergeWith :: [Patch] -> MergeResult
    mergeWith ps =
        let ops    = map toMap ps
            allOps = mapReduce (M.unionWith (++)) ops
            ignore = M.filter (\l -> 1 < length l) allOps
            accept = M.difference allOps ignore
        in (fromMap accept, concat $ M.elems ignore)

    fromMap :: M.Map D.Path [RejectedOp] -> Patch
    fromMap = foldr addOperation emptyPatch . map (^. rejectedOperation) . concat . M.elems

    mapReduce _ [] = mempty
    mapReduce _ [m] = m
    mapReduce f (h:r) = f h (mapReduce f r)

    -- Groups changes in a patch by the change path.
    toMap  :: Patch -> M.Map D.Path [RejectedOp]
    toMap p = foldr count M.empty $ reject p

    count :: RejectedOp -> M.Map D.Path [RejectedOp] -> M.Map D.Path [RejectedOp]
    count o = M.insertWith (++) (D.changePath $ o ^. rejectedOperation) [o]

--------------------------------------------------------------------------------

-- | Use a 'MergePolicy' to compare two versions of a document and extract a
-- 'Patch' describing changes.
diff
    :: MergePolicy
    -> Document
    -> Document
    -> Patch
diff MergePolicy{..} d1 d2 =
    let j1 = d1 ^. documentContent
        j2 = d2 ^. documentContent
        l = extractLabel d2
        d = D.diff j1 j2
    in Patch l d

-- | Use a 'MergePolicy' to apply a 'Patch' to a 'Document'.
patch
    :: MergePolicy
    -> Patch
    -> Document
    -> Document
patch MergePolicy{} p d = d & documentContent %~ D.patch (p ^. patchDiff)

-- | Use a 'MergePolicy' to merge a pair of 'Patch'es.
merge
    :: MergePolicy
    -> Patch
    -> Patch
    -> MergeResult
merge pol p1 p2 = mergeWith pol [p1, p2]

--------------------------------------------------------------------------------

-- * Utility

-- | Convert all the 'D.Operation's in a 'D.Patch' into 'RejectedOp's.
reject :: Patch -> [RejectedOp]
reject p = fmap (RejectedOp (p ^. patchLabel)) . D.patchOperations $ p ^. patchDiff

addOperation :: D.Operation -> Patch -> Patch
addOperation x p = p & patchDiff . patchOperations <>~ [x]

patchOperations :: Lens' D.Patch [D.Operation]
patchOperations f (D.Patch os) = D.Patch <$> f os
