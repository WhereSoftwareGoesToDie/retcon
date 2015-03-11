{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}


module Synchronise.Store where

import Control.Applicative
import Control.Lens
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Control.Monad
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
data Store store = Store
  { -- | Initialise a handle to the storage backend.
    initBackend  :: IO store

    -- | Finalise a handle to the storage backend.
  , closeBackend :: store -> IO ()

    -- | clone a store handle for concurrent operations. the new handle must be
    -- safe to use concurrently with the original handle.
    --
    -- (e.g. open a second connection to the same postgresql database.)
    --
    -- this may be a no-op for backends which are already thread-safe (e.g.
    -- in-memory iorefs).
  , cloneStore   :: store -> IO (Store store)

    -- Operations on interal keys

    -- | Allocate and return a new 'InternalKey'.
  , createInternalKey :: store -> EntityName -> IO InternalKey

    -- | Find the 'InternalKey' associated with a 'ForeignKey'.
  , lookupInternalKey :: store -> ForeignKey -> IO (Maybe InternalKey)

    -- | Delete an 'InternalKey' and any associated resources.
  , deleteInternalKey :: store -> InternalKey -> IO Int


    -- Operations on foreign keys

    -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
  , recordForeignKey :: store -> InternalKey -> ForeignKey -> IO ()

    -- | Find the 'ForeignKey' corresponding to an 'InternalKey'
  , lookupForeignKey :: store -> SourceName -> InternalKey -> IO (Maybe ForeignKey)

    -- | Delete a 'ForeignKey'.
  , deleteForeignKey :: store -> ForeignKey  -> IO Int

    -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
  , deleteForeignKeysWithInternal
      :: store -> InternalKey -> SourceName -> IO Int


    -- Operations on initial documents

    -- | Record the initial 'Document' associated with an 'InternalKey'.
  , recordInitialDocument :: store -> InternalKey -> Document -> IO ()

    -- | Lookup the initial 'Document', if any, associated with an 'InternalKey'.
  , lookupInitialDocument :: store -> InternalKey -> IO (Maybe Document)

    -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
  , deleteInitialDocument :: store -> InternalKey -> IO Int


    -- Operations on patches

    -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
    --   processed 'InternalKey'.
  , recordDiffs
      :: forall label. store
      -> InternalKey
      -> (LabelledPatch label, [LabelledPatch label])
      -> IO DiffID 

    -- | Record that the conflicts in a 'Diff' are resolved.
  , resolveDiffs        :: store -> Int -> IO ()

    -- | Lookup the list of 'Diff' IDs associated with an 'InternalKey'.
  , lookupDiffIDs       :: store -> InternalKey -> IO [DiffID]

    -- | Lookup the list of conflicted 'Diff's with related information.
  , lookupConflicts     :: store -> IO [ConflictResp]

    -- | Lookup the merged and conflicting 'Diff's with a given ID.
  , lookupDiff          :: store -> DiffID -> IO (Maybe DiffResp)

    -- | Lookup the specified 'DiffOp's from the data store.
  , lookupDiffConflicts :: forall label. store -> [OpID] -> IO [OpResp label]

    -- | Delete the 'Diff', if any, with a given ID.
  , deleteDiff          :: store -> DiffID -> IO Int

    -- | Delete the 'Diff's associated with an 'InternalKey'.
  , deleteDiffsWithKey  :: store -> InternalKey -> IO Int


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

  
-- | An acid-state like in-memory store.
type Mem = IORef MemStore

data MemStore = MemStore
    { _memNextKey :: Int
    , _memItoF    :: Map InternalKey (Map SourceName ForeignID)
    , _memFtoI    :: Map ForeignKey InternalID
    , _memInits   :: Map InternalKey Document
    , _memDiffs   :: Map InternalKey [(LabelledPatch (), [LabelledPatch ()])]
    }
makeLenses ''MemStore

-- | "Open the module" with the in-memory store type.
--
memoryStore :: Store Mem
memoryStore = Store
  -- yes it needs to be this terrible because we're not an actual module
  _initBackend 
  _closeBackend 
  _cloneStore 
  _createInternalKey
  _lookupInternalKey
  _deleteInternalKey
  _recordForeignKey
  _lookupForeignKey 
  _deleteForeignKey
  _deleteForeignKeysWithInternal
  _recordInitialDocument
  _lookupInitialDocument
  _deleteInitialDocument
  _recordDiffs
  _resolveDiffs
  _lookupDiffIDs
  undefined -- NOT DEFINED IN RETCON
  _lookupDiff
  undefined -- NOT DEFINED
  _deleteDiff
  undefined -- NOT DEFINED
  _addWork
  _getWork
  _completeWork


  where emptyMem = MemStore 0 mempty mempty mempty mempty 

        _initBackend   = newIORef emptyMem

        _closeBackend  = flip writeIORef emptyMem

        _cloneStore ref = return ( memoryStore { initBackend = return ref} )

        _createInternalKey ref entity =
          atomicModifyIORef' ref $ \st ->
            (memNextKey +~ 1 $ st, InternalKey entity (st ^. memNextKey))

        _lookupInternalKey ref fk = do
          st <- readIORef ref
          return $ st ^? memFtoI . ix fk . to (InternalKey (fkEntity fk)) 

        _deleteInternalKey ref ik =
          atomicModifyIORef' ref $ \st ->
              (st & memItoF . at ik .~ Nothing, 0)

        _recordForeignKey ref ik fk =
          let source_name = fkSource fk
              foreign_id  = fkID     fk
              internal_id = ikID     ik
          in  atomicModifyIORef' ref $ \st ->
                ( st & memItoF . at ik . non mempty . at source_name ?~ foreign_id
                     & memFtoI . at fk ?~ internal_id
                , ())

        _lookupForeignKey ref source_name ik = do
            let entity_name = ikEntity ik
            st <- readIORef ref
            -- Construct a new ForeignKey if it exists in the memItoF map
            return $ st ^? memItoF . ix ik . ix source_name . to (ForeignKey entity_name source_name)

        _deleteForeignKey ref fk = do
          maybe_ik <- _lookupInternalKey ref fk
          atomicModifyIORef' ref $ \st ->
              -- Go through the internal to foreign map removing all foreign ids
              -- that match this value under the associated internal key.
              ( st & maybe id (\iki -> memItoF . ix iki %~ M.filter (/= fkID fk)) maybe_ik
              -- Also, delete the foreign in the foreign to internal map.
                   & memFtoI . at fk .~ Nothing
              , 0)

        _deleteForeignKeysWithInternal ref ik _ = do
            let entity_name = ikEntity ik
            atomicModifyIORef' ref $ \st ->
                -- List of the foreign key identifiers associated with the internal
                -- key
                let fkis = map (uncurry (ForeignKey entity_name)) $
                            st ^.. memItoF . at ik . _Just . itraversed . withIndex in
                -- Now delete the foreign keys from the foreign to internal map
                ( st & memFtoI %~ (\ftoi -> foldr M.delete ftoi fkis)
                , ())
            -- Now remove the internal key.
            _deleteInternalKey ref ik


        _recordInitialDocument ref ik doc =
            atomicModifyIORef' ref $ \st ->
                (st & memInits . at ik ?~ doc, ())

        _lookupInitialDocument ref ik =
            (^. memInits . at ik) <$> readIORef ref

        _deleteInitialDocument ref ik =
          atomicModifyIORef' ref $ \st ->
            (st & memInits . at ik .~ Nothing, 0)

        _recordDiffs ref ik new =
            let relabeled = bimap void (map void) new in
            atomicModifyIORef' ref $ \st ->
                -- Slow due to list traversal
                (st & memDiffs . at ik . non mempty <%~ (++[relabeled]))
                ^. swapped & _2 %~ length

        -- this is defined in the class but not implmented???
        _resolveDiffs = undefined

        -- TODO Implement
        _lookupDiff ref _ =
          let get st = (st, Nothing)
          in  atomicModifyIORef' ref get

        -- TODO Implement
        _lookupDiffIDs ref _ =
          let get st = (st, [])
          in  atomicModifyIORef' ref get

        -- TODO: Implement
        _deleteDiff ref _ =
          let del st = (st, 0)
          in  atomicModifyIORef' ref del

        _deleteDiffsWithKey ref ik =
            atomicModifyIORef' ref $ \st ->
                (st & memDiffs . at ik .~ Nothing, 0)

        _addWork      = const $ const $ return ()
        _getWork      = const $ return Nothing
        _completeWork = const $ const $ return ()
