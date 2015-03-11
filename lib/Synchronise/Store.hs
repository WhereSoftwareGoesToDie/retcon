{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}

module Synchronise.Store where

import Control.Lens
import Data.ByteString (ByteString)

import Synchronise.Diff
import Synchronise.Document
import Synchronise.Identifier

type DiffID  = Int
type OpID    = Int

data ConflictResp = ConflictResp
  { _conflictRawDoc  :: ByteString
  , _conflictRawDiff :: ByteString
  , _conflictDiffID  :: DiffID
  , _conflictRawOps  :: [(OpID, ByteString)]
  }
makeLenses ''ConflictResp

data DiffResp = DiffResp
  { _diffEntity    :: ByteString
  , _diffKey       :: Int
  , _diffPatch     :: LabelledPatch ()
  , _diffConflicts :: [LabelledPatch ()]
  }
makeLenses ''DiffResp

data OpResp label = OpResp 
  { _opDiffID :: DiffID
  , _opID     :: OpID
  , _ops      :: LabelledOp label
  }
makeLenses ''OpResp

-- | The internal store "module".
--
data Store store = Store 
  { -- Operations on interal keys

    -- | Allocate and return a new 'InternalKey'.
    createInternalKey :: store -> IO InternalKey

    -- | Find the 'InternalKey' associated with a 'ForeignKey'.
  , lookupInternalKey :: store -> ForeignKey  -> IO (Maybe InternalKey)

    -- | Delete an 'InternalKey' and any associated resources.
  , deleteInternalKey :: store -> InternalKey -> IO Int


    -- Operations on foreign keys

    -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
  , recordForeignKey :: store -> InternalKey -> ForeignKey -> IO ()

    -- | Find the 'ForeignKey' corresponding to an 'InternalKey'
  , lookupForeignKey :: store -> InternalKey -> IO (Maybe ForeignKey)

    -- | Delete a 'ForeignKey'.
  , deleteForeignKey :: store -> ForeignKey  -> IO Int

    -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
  , deleteForeignKeyWithInternal
      :: store -> InternalKey -> IO Int


    -- Operations on initial documents

    -- | Record the initial 'Document' associated with an 'InternalKey'.
  , recordDocument :: store -> InternalKey -> Document -> IO ()

    -- | Lookup the initial 'Document', if any, associated with an 'InternalKey'.
  , lookupDocument :: store -> InternalKey -> IO (Maybe Document)

    -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
  , deleteDocument :: store -> InternalKey -> IO Int


    -- Operations on patches

    -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
    --   processed 'InternalKey'.
  , recordDiffs
      :: forall label. store
      -> InternalKey -> LabelledPatch label -> [LabelledPatch label] -> IO Int

    -- | Record that the conflicts in a 'Diff' are resolved.
  , resolveDiffs :: store -> Int -> IO ()

    -- | Lookup the list of 'Diff' IDs associated with an 'InternalKey'.
  , lookupDiffIDs :: store -> InternalKey -> IO [DiffID]

    -- | Lookup the list of conflicted 'Diff's with related information.
  , lookupConflicts :: store -> IO [ConflictResp]

    -- | Lookup the merged and conflicting 'Diff's with a given ID.
  , lookupDiff :: store -> DiffID -> IO (Maybe DiffResp)

    -- | Lookup the specified 'DiffOp's from the data store.
  , lookupDiffConflicts :: forall label. store -> [OpID] -> IO [OpResp label]

    -- | Delete the 'Diff', if any, with a given ID.
  , deleteDiff :: store -> DiffID -> IO Int

    -- | Delete the 'Diff's associated with an 'InternalKey'.
  , deleteDiffsWithKey :: store -> InternalKey -> IO Int


    -- Operations on notifications

    -- | Record a 'Notification' associated with a given 'InternalKey'
    -- and 'Diff' ID.
  , recordNotification :: store -> InternalKey -> DiffID -> IO ()

    -- | Fetch and delete 'Notification's from the store.
  , fetchNotifications
        :: store
        -> Int                      -- ^ Maximum number to return.
        -> IO (Int, [Notification]) -- ^ Number of leftover notifcations in the store and fetched notifications


    -- Operation on store work queue
        
    -- | Add a work item to the work queue.
  , addWork :: store -> WorkItem -> IO ()

    -- | Get a work item from the work queue.
    --
    -- The item will be locked for a period of time, after which it will become
    -- available for other workers to claim.
    --
    -- TODO: The period of time is currently hardcoded in the implementations.
  , getWork :: store -> IO (Maybe (WorkItemID, WorkItem))

    -- | Remove a completed work item from the queue.
  , completeWork :: store -> WorkItemID -> IO ()
  }
