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

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import qualified Data.Aeson.Diff     as D
import           Data.Either
import           Data.List
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
--   Changes will be applied iff changes for that key appear in only one diff;
--   all changes to keys which appear in multiple diffs will be rejected.
ignoreConflicts :: MergePolicy
ignoreConflicts = MergePolicy {..}
  where
    extractLabel = const Unamed
    mergeWith :: [Patch] -> MergeResult
    mergeWith patches =
        let patch_keys = map (nub . sort . map D.changePath . D.patchOperations . _patchDiff) patches
            touched = group . sort . concat $ patch_keys
            accepted_paths = map head . filter (\l -> length l == 1) $ touched
            allOps = reject =<< patches
            (rejected, accepted) = partitionEithers . map (filterChanges accepted_paths) $ allOps
        in (makePatch accepted, rejected)

    -- | Filter out rejected changes.
    filterChanges :: [D.Path] -> RejectedOp -> Either RejectedOp RejectedOp
    filterChanges acc rop =
        if (D.changePath $ rop ^. rejectedOperation) `elem` acc
            then Right rop
            else Left rop

    makePatch = foldr addOperation emptyPatch . map (^. rejectedOperation)

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
addOperation x p = p & patchDiff . patchOperations %~ (x:)

patchOperations :: Lens' D.Patch [D.Operation]
patchOperations f (D.Patch os) = D.Patch <$> f os
