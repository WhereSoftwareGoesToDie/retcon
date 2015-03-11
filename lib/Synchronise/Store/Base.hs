{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}

module Synchronise.Store.Base where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Text (Text)

import Synchronise.Diff
import Synchronise.Document
import Synchronise.Identifier


-- TODO
data WorkItem
data WorkItemID
data Diff


type DiffID  = Int
type OpID    = Int

type InternalID = Int
type ForeignID  = Text

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
class Store store where
  -- | Initialise a handle to the storage backend.
  initBackend  :: IO store

  -- | Finalise a handle to the storage backend.
  closeBackend :: store -> IO ()

  -- | clone a store handle for concurrent operations. the new handle must be
  -- safe to use concurrently with the original handle.
  --
  -- (e.g. open a second connection to the same postgresql database.)
  --
  -- this may be a no-op for backends which are already thread-safe (e.g.
  -- in-memory iorefs).
  cloneStore   :: store -> IO store

  -- Operations on interal keys

  -- | Allocate and return a new 'InternalKey'.
  createInternalKey :: store -> EntityName -> IO InternalKey

  -- | Find the 'InternalKey' associated with a 'ForeignKey'.
  lookupInternalKey :: store -> ForeignKey -> IO (Maybe InternalKey)

  -- | Delete an 'InternalKey' and any associated resources.
  deleteInternalKey :: store -> InternalKey -> IO Int


  -- Operations on foreign keys

  -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
  recordForeignKey :: store -> InternalKey -> ForeignKey -> IO ()

  -- | Find the 'ForeignKey' corresponding to an 'InternalKey'
  lookupForeignKey :: store -> SourceName -> InternalKey -> IO (Maybe ForeignKey)

  -- | Delete a 'ForeignKey'.
  deleteForeignKey :: store -> ForeignKey  -> IO Int

  -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
  deleteForeignKeysWithInternal
    :: store -> InternalKey -> SourceName -> IO Int


  -- Operations on initial documents

  -- | Record the initial 'Document' associated with an 'InternalKey'.
  recordInitialDocument :: store -> InternalKey -> Document -> IO ()

  -- | Lookup the initial 'Document', if any, associated with an 'InternalKey'.
  lookupInitialDocument :: store -> InternalKey -> IO (Maybe Document)

  -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
  deleteInitialDocument :: store -> InternalKey -> IO Int


  -- Operations on patches

  -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
  --   processed 'InternalKey'.
  recordDiffs
    :: forall label. store
    -> InternalKey
    -> (LabelledPatch label, [LabelledPatch label])
    -> IO DiffID 

  -- | Record that the conflicts in a 'Diff' are resolved.
  resolveDiffs        :: store -> Int -> IO ()

  -- | Lookup the list of 'Diff' IDs associated with an 'InternalKey'.
  lookupDiffIDs       :: store -> InternalKey -> IO [DiffID]

  -- | Lookup the list of conflicted 'Diff's with related information.
  lookupConflicts     :: store -> IO [ConflictResp]

  -- | Lookup the merged and conflicting 'Diff's with a given ID.
  lookupDiff          :: store -> DiffID -> IO (Maybe DiffResp)

  -- | Lookup the specified 'DiffOp's from the data store.
  lookupDiffConflicts :: forall label. store -> [OpID] -> IO [OpResp label]

  -- | Delete the 'Diff', if any, with a given ID.
  deleteDiff          :: store -> DiffID -> IO Int

  -- | Delete the 'Diff's associated with an 'InternalKey'.
  deleteDiffsWithKey  :: store -> InternalKey -> IO Int


  -- Operation on store work queue
      
  -- | Add a work item to the work queue.
  addWork :: store -> WorkItem -> IO ()

  -- | Get a work item from the work queue.
  --
  -- The item will be locked for a period of time, after which it will become
  -- available for other workers to claim.
  --
  -- TODO: The period of time is currently hardcoded in the implementations.
  getWork :: store -> IO (Maybe (WorkItemID, WorkItem))

  -- | Remove a completed work item from the queue.
  completeWork :: store -> WorkItemID -> IO ()
  

  
