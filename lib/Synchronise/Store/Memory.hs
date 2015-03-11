
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}

module Synchronise.Store.Memory where


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


import Synchronise.Store.Base
       
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
  undefined -- NOT DEFINED IN RETCON
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
