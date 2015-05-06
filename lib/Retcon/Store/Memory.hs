--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Description: In-memory storage for operational data.
--
-- Retcon maintains quite a lot of operational data. This implements the
-- operational data storage interface using an in-memory data structure. This
-- is useful for test suites, demonstrations, etc.
module Retcon.Store.Memory where

import Control.Lens
import Control.Monad
import Data.Functor
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import GHC.TypeLits

import Retcon.Core
import Retcon.Diff
import Retcon.Document
import Retcon.Notifications

-- | Collection of in-memory data-structures to store retcon internal state.
data MemoryStore = MemoryStore
    { _memNextKey :: Int
    , _memItoF    :: Map InternalKeyIdentifier (Map SourceName ForeignID)
    , _memFtoI    :: Map ForeignKeyIdentifier InternalID
    , _memInits   :: Map InternalKeyIdentifier Document
    , _memDiffs   :: Map InternalKeyIdentifier [(Diff (), [Diff ()])]
    , _memNotes   :: [Notification]
    }
  deriving (Show)
makeLenses ''MemoryStore

-- | An empty 'MemoryStore' value.
emptyMemoryStore :: MemoryStore
emptyMemoryStore = MemoryStore 0 mempty mempty mempty mempty mempty

-- | An ephemeral in-memory storage backend for Retcon.
newtype MemStorage = MemStorage { unwrapMemStorage :: IORef MemoryStore }

-- | Ephemeral in-memory data storage.
instance RetconStore MemStorage where

    storeInitialise _ = MemStorage <$> newIORef emptyMemoryStore

    storeFinalise (MemStorage ref) = writeIORef ref emptyMemoryStore

    storeClone (MemStorage ref) = return . MemStorage $ ref

    storeCreateInternalKey (MemStorage ref) =
        atomicModifyIORef' ref $ \st ->
            (memNextKey +~ 1 $ st, InternalKey $ st ^. memNextKey)

    storeLookupInternalKey (MemStorage ref) fk = do
        st <- readIORef ref
        return $ st ^? memFtoI . ix (foreignKeyValue fk) . to InternalKey

    storeDeleteInternalKey (MemStorage ref) ik =
        atomicModifyIORef' ref $ \st ->
            (st & memItoF . at (internalKeyValue ik) .~ Nothing, 0)

    storeRecordForeignKey (MemStorage ref) ik fk = do
        let iki@(_, internal_id) = internalKeyValue ik
        let fki@(_,source_name, foreign_id) = foreignKeyValue fk
        atomicModifyIORef' ref $ \st ->
            ( st & memItoF . at iki . non mempty . at source_name ?~ foreign_id
                 & memFtoI . at fki ?~ internal_id
            , ())

    storeLookupForeignKey
        :: forall entity source. RetconDataSource entity source
        => MemStorage
        -> InternalKey entity
        -> IO (Maybe (ForeignKey entity source))
    storeLookupForeignKey (MemStorage ref) ik = do
        st <- readIORef ref
        -- Acquire the SourceName from the inferred type
        let source = symbolVal (Proxy :: Proxy source)
        -- Acquire the InternalKeyIdentifier from the inferred type
        let iki    = internalKeyValue ik
        -- Construct a new ForeignKey if it exists in the memItoF map
        return $ st ^? memItoF . ix iki . ix source . to ForeignKey

    storeDeleteForeignKey store@(MemStorage ref) fk = do
        let fki@(_,_, foreign_id) = foreignKeyValue fk
        maybe_iki <- (fmap . fmap) internalKeyValue (storeLookupInternalKey store fk)
        atomicModifyIORef' ref $ \st ->
            -- Go through the internal to foreign map removing all foreign ids
            -- that match this value under the associated internal key.
            ( st & maybe id (\iki -> memItoF . ix iki %~ M.filter (/= foreign_id)) maybe_iki
            -- Also, delete the foreign in the foreign to internal map.
                 & memFtoI . at fki .~ Nothing
            , 0)

    storeDeleteForeignKeys store@(MemStorage ref) ik = do
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

    storeRecordInitialDocument (MemStorage ref) ik doc =
        atomicModifyIORef' ref $ \st ->
            (st & memInits . at (internalKeyValue ik) ?~ doc, ())

    storeLookupInitialDocument (MemStorage ref) ik =
        (^. memInits . at (internalKeyValue ik)) <$> readIORef ref

    storeDeleteInitialDocument (MemStorage ref) ik =
        atomicModifyIORef' ref $ \st ->
            (st & memInits . at (internalKeyValue ik) .~ Nothing, 0)

    storeRecordDiffs (MemStorage ref) ik new =
        let relabeled = bimap void (map void) new in
        atomicModifyIORef' ref $ \st ->
            -- Slow due to list traversal
            (st & memDiffs . at (internalKeyValue ik) . non mempty <%~ (++[relabeled]))
            ^. swapped & _2 %~ length

    -- TODO Implement
    storeLookupDiff (MemStorage ref) _did =
        atomicModifyIORef' ref get
      where
        get st = (st, Nothing)

    -- TODO Implement
    storeLookupDiffIds (MemStorage ref) _ik =
        atomicModifyIORef' ref get
      where
        get st = (st, [])

    -- TODO: Implement
    storeDeleteDiff (MemStorage ref) _did =
        atomicModifyIORef' ref del
      where
        del st = (st, 0)

    storeDeleteDiffs (MemStorage ref) ik =
        atomicModifyIORef' ref $ \st ->
            (st & memDiffs . at (internalKeyValue ik) .~ Nothing, 0)

    storeRecordNotification (MemStorage ref) ik did = do
        t <- getCurrentTime
        let (entity, key) = internalKeyValue ik
        let note = Notification t (T.pack entity) key did
        atomicModifyIORef' ref $ \st ->
            (st & memNotes %~ (++ [note]), ())

    storeFetchNotifications (MemStorage ref) limit =
        atomicModifyIORef' ref $ \st ->
            let (del, keep) = splitAt limit $ st ^. memNotes
                remaining = length keep
            in (st & memNotes .~ keep, (remaining, del))

    storeResolveDiff = error "Retcon.Store.Memory: storeResolveDiff not supported."

    storeLookupConflicts = error "Retcon.Store.Memory: storeLookupConflicts not supported."

    storeLookupDiffConflicts = error "Retcon.Store.Memory: storeLookupDiffConflicts not supported."

    storeAddWork = error "Retcon.Store.Memory: storeAddWork not supported."

    storeGetWork = error "Retcon.Store.Memory: storeGetWork not supported."

    storeCompleteWork = error "Retcon.Store.Memory: storeCompleteWork not supported."
