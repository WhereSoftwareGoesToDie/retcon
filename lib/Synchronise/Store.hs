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
  { -- | Initialise a handle to the storage backend.
    initBackend  :: store -> IO ()

    -- | Finalise a handle to the storage backend.
  , closeBackend :: store -> IO ()

    -- | Brackets an action with init/finalise store.
  , withStore    :: store -> IO a -> IO a

    -- | clone a store handle for concurrent operations. the new handle must be
    -- safe to use concurrently with the original handle.
    --
    -- (e.g. open a second connection to the same postgresql database.)
    --
    -- this may be a no-op for backends which are already thread-safe (e.g.
    -- in-memory iorefs).
  , cloneStore   :: store -> IO (StoreOp store)

    -- Operations on interal keys

    -- | Allocate and return a new 'InternalKey'.
  , createInternalKey :: store -> IO InternalKey

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
      -> InternalKey
      -> LabelledPatch label
      -> [LabelledPatch label]
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

  
-- | An acid-state like in-memory store.
type Mem = IORef MemStore

data MemStore = MemStore
    { _memNextKey :: Int
    , _memItoF    :: Map InternalKeyIdentifier (Map SourceName ForeignID)
    , _memFtoI    :: Map ForeignKeyIdentifier InternalID
    , _memInits   :: Map InternalKeyIdentifier Document
    , _memDiffs   :: Map InternalKeyIdentifier [(Diff (), [Diff ()])]
    , _memNotes   :: [Notification]
    }
  deriving (Show)
makeLenses ''Mem


-- | "Open the module" with the in-memory store type.
--
memoryStore :: Store Mem
memoryStore = Store
  { initBackend   = Mem <$> newIORef emptyMem

  , storeFinalise = flip writeIORef emptyMem

  , cloneStore    = return

  , createInternalKey = \ref ->
      atomicModifyIORef' ref $ \st ->
        (memNextKey +~ 1 $ st, InternalKey $ st ^. memNextKey)

  , lookupInternalKey = \ref ik -> do
      st <- readIORef ref
      return $ st ^? memFtoI . ix (foreignKeyValue fk) . to InternalKey

  , deleteInternalKey = \ref ik -> 
      atomicModifyIORef' ref $ \st ->
          (st & memItoF . at (internalKeyValue ik) .~ Nothing, 0)


  , recordForeignKey = \ref ik fk -> do
        let iki@(_, internal_id) = internalKeyValue ik
        let fki@(_,source_name, foreign_id) = foreignKeyValue fk
        atomicModifyIORef' ref $ \st ->
            ( st & memItoF . at iki . non mempty . at source_name ?~ foreign_id
                 & memFtoI . at fki ?~ internal_id
            , ())

  , lookupForeignKey = \ref ik -> do
        st <- readIORef ref
        -- Acquire the SourceName from the inferred type
        let source = symbolVal (Proxy :: Proxy source)
        -- Acquire the InternalKeyIdentifier from the inferred type
        let iki    = internalKeyValue ik
        -- Construct a new ForeignKey if it exists in the memItoF map
        return $ st ^? memItoF . ix iki . ix source . to ForeignKey

  , deleteForeignKey = \ref fk -> do
        let fki@(_,_, foreign_id) = foreignKeyValue fk
        maybe_iki <- (fmap . fmap) internalKeyValue (storeLookupInternalKey store fk)
        atomicModifyIORef' ref $ \st ->
            -- Go through the internal to foreign map removing all foreign ids
            -- that match this value under the associated internal key.
            ( st & maybe id (\iki -> memItoF . ix iki %~ M.filter (/= foreign_id)) maybe_iki
            -- Also, delete the foreign in the foreign to internal map.
                 & memFtoI . at fki .~ Nothing
            , 0)

   , deleteForeignKeysWithInternal = \ref ik -> do
         let iki@(entity_name, _) = internalKeyValue ik
         atomicModifyIORef' ref $ \st ->
             -- List of the foreign key identifiers associated with the internal
             -- key
             let fkis = map (\(source, f_id) -> (entity_name, source, f_id)) $
                         st ^.. memItoF . at iki . _Just . itraversed . withIndex in
             -- Now delete the foreign keys from the foreign to internal map
             ( st & memFtoI %~ (\ftoi -> foldr M.delete ftoi fkis)
             , ())
         -- Now remove the internal key.
         storeDeleteInternalKey store ik

   , recordDocument = \ref ik doc ->
         atomicModifyIORef' ref $ \st ->
             (st & memInits . at (internalKeyValue ik) ?~ doc, ())

   , lookupDocument = \ref ik -> 
         (^. memInits . at (internalKeyValue ik)) <$> readIORef ref

   , deleteInitialDocument = \ref ik ->
       atomicModifyIORef' ref $ \st ->
         (st & memInits . at (internalKeyValue ik) .~ Nothing, 0)


   , recordDiffs = \ref ik new ->
         let relabeled = bimap void (map void) new in
         atomicModifyIORef' ref $ \st ->
             -- Slow due to list traversal
             (st & memDiffs . at (internalKeyValue ik) . non mempty <%~ (++[relabeled]))
             ^. swapped & _2 %~ length

     -- this is defined in the class but not implmented???
   , resolveDiffs = undefined

     -- TODO Implement
   , lookupDiff = \ref did ->
       let get st = (st, Nothing)
       in  atomicModifyIORef' ref get

     -- TODO Implement
   , lookupDiffIds  = \ref ik ->
       let get st = (st, [])
       in  atomicModifyIORef' ref get

     -- TODO: Implement
   , deleteDiff = \ref did ->
       let del st = (st, 0)
       in  atomicModifyIORef' ref del

   , deleteDiffsWithKey = \ref ik ->
         atomicModifyIORef' ref $ \st ->
             (st & memDiffs . at (internalKeyValue ik) .~ Nothing, 0)

   , recordNotification = \ref ik did -> do
         t <- getCurrentTime
         let (entity, key) = internalKeyValue ik
         let note = Notification t (T.pack entity) key did
         atomicModifyIORef' ref $ \st ->
             (st & memNotes %~ (++ [note]), ())

   , fetchNotifications = \ref limit ->
         atomicModifyIORef' ref $ \st ->
             let (del, keep) = splitAt limit $ st ^. memNotes
                 remaining = length keep
             in (st & memNotes .~ keep, (remaining, del))

    , addWork      = const $ const $ return ()
    , getWork      = const $ return Nothing
    , completeWork = const $ const $ return ()
  }

  where emptyMem = MemoryStore 0 mempty mempty mempty mempty mempty


