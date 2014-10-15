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
{-# LANGUAGE TypeFamilies          #-}

-- | Description: In-memory storage for operational data.
--
-- Retcon maintains quite a lot of operational data. This implements the
-- operational data storage interface using an in-memory data structure. This
-- is useful for test suites, demonstrations, etc.
module Retcon.Store.Memory where

import Control.Monad
import Data.Bifunctor
import Data.Functor
import Data.IORef
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Data.Type.Equality
import GHC.TypeLits

import Retcon.DataSource
import Retcon.Diff
import Retcon.Document
import Retcon.Notifications

-- | Convert a triple of strings into a ForeignKey iff the entity and
-- source names match the expected type.
--
-- What is this witchery?!?
convert
    :: forall (entity :: Symbol) (source :: Symbol).
       (RetconDataSource entity source)
    => Proxy entity
    -> Proxy source
    -> (String, String, String)
    -> Maybe (ForeignKey entity source)
convert e s (en, sn, k) =
    case (someSymbolVal en, someSymbolVal sn) of
        (SomeSymbol es, SomeSymbol ss) ->
            case (sameSymbol e es, sameSymbol s ss) of
                (Just Refl, Just Refl) -> Just . ForeignKey $ k
                _ -> Nothing

-- | Collection of in-memory data-structures to store retcon internal state.
data State = MemoryStore
    { memNextKey :: Int
    , memItoF    :: Map Int [(String, String, String)]
    , memFtoI    :: Map (String, String, String) Int
    , memInits   :: Map (String, Int) Document
    , memDiffs   :: Map (String, Int) [(Diff (), [Diff ()])]
    , memNotes   :: [Notification]
    }
  deriving (Eq, Show)

-- | An empty 'State' value.
emptyState :: State
emptyState = MemoryStore nextKey i2fMap f2iMap docs diffs notes
  where
    nextKey = 0
    i2fMap = M.empty
    f2iMap = M.empty
    docs   = M.empty
    diffs  = M.empty
    notes  = []

-- | An ephemeral in-memory storage backend for Retcon.
newtype MemStorage = MemStorage { unwrapMemStorage :: IORef State }

-- | Ephemeral in-memory data storage.
instance RetconStore MemStorage where

    storeInitialise _ = MemStorage <$> newIORef emptyState

    storeFinalise (MemStorage ref) = writeIORef ref emptyState

    storeCreateInternalKey (MemStorage ref) =
        atomicModifyIORef' ref alloc
      where
        alloc st = let n = memNextKey st
                       st' = st { memNextKey = n + 1 }
                   in (st', InternalKey n)

    storeLookupInternalKey (MemStorage ref) fk =
        atomicModifyIORef' ref get
      where
        get st = let f2i = memFtoI st
                     k = foreignKeyValue fk
                     ik = InternalKey <$> M.lookup k f2i
                 in (st, ik)

    storeDeleteInternalKey (MemStorage ref) ik =
        atomicModifyIORef' ref del
      where
        del st = let i2f = memItoF st
                     f2i = memFtoI st
                     k = unInternalKey ik
                     st' = st { memItoF = M.delete k i2f
                              , memFtoI = f2i
                              }
                 in (st', ())

    storeRecordForeignKey (MemStorage ref) ik fk =
        atomicModifyIORef' ref record
      where
        alter v Nothing = Just [v]
        alter v (Just l) = Just . nub . sort $ v:l
        record st = let i2f = memItoF st
                        f2i = memFtoI st
                        ikv = unInternalKey ik
                        fkv = foreignKeyValue fk
                        st' = st { memItoF = M.alter (alter fkv) ikv i2f
                                 , memFtoI = M.insert fkv ikv f2i
                                 }
                    in (st', ())

    storeLookupForeignKey
        :: forall (e :: Symbol) (d :: Symbol). RetconDataSource e d
        => MemStorage
        -> InternalKey e
        -> IO (Maybe (ForeignKey e d))
    storeLookupForeignKey (MemStorage ref) ik =
        atomicModifyIORef' ref get
      where
        get st = let i2f = memItoF st
                     k = unInternalKey ik
                     e = Proxy :: Proxy e
                     s = Proxy :: Proxy d
                     fks = M.lookup k i2f
                     fk = join $ listToMaybe . mapMaybe (convert e s) <$> fks
                 in (st, fk)

    storeDeleteForeignKey (MemStorage ref) fk =
        atomicModifyIORef' ref del
      where
        del st = let i2f = memItoF st
                     f2i = memFtoI st
                     fkv = foreignKeyValue fk
                     ik = M.lookup fkv f2i
                     i2f' = maybe i2f (\k -> M.update (Just . delete fkv) k i2f) ik
                     f2i' = M.delete fkv f2i
                     st' = st { memItoF = i2f'
                              , memFtoI = f2i'
                              }
                 in (st', ())

    storeDeleteForeignKeys (MemStorage ref) ik =
        atomicModifyIORef' ref del
      where
        del st = let i2f = memItoF st
                     f2i = memFtoI st
                     ikv = unInternalKey ik
                     fks = M.lookup ikv i2f
                     i2f' = M.delete ikv i2f
                     f2i' = maybe f2i (foldr M.delete f2i) fks
                     st' = st { memItoF = i2f'
                              , memFtoI = f2i'
                              }
                 in (st', ())

    storeRecordInitialDocument (MemStorage ref) ik doc =
        atomicModifyIORef' ref update
      where
        update st = let ids = memInits st
                        ikv = internalKeyValue ik
                        ids' = M.insert ikv doc ids
                        st' = st { memInits = ids' }
                    in (st', ())

    storeLookupInitialDocument (MemStorage ref) ik =
        atomicModifyIORef' ref get
      where
        get st = let ids = memInits st
                     ikv = internalKeyValue ik
                     v = M.lookup ikv ids
                 in (st, v)

    storeDeleteInitialDocument (MemStorage ref) ik =
        atomicModifyIORef' ref del
      where
        del st = let ids = memInits st
                     ikv = internalKeyValue ik
                     ids' = M.delete ikv ids
                     st' = st { memInits = ids' }
                 in (st', ())

    storeRecordDiffs (MemStorage ref) ik new =
        atomicModifyIORef' ref ins
      where
        ins st = let new' = bimap void (map void) new
                     ds = memDiffs st
                     ikv = internalKeyValue ik
                     ds' = M.alter (Just . maybe [new'] (++[new'])) ikv ds
                     did = length $ maybe [] id $ M.lookup ikv ds'
                     st' = st { memDiffs = ds' }
                 in (st', did)

    -- TODO Implement
    storeLookupDiff (MemStorage ref) did =
        atomicModifyIORef' ref get
      where
        get st = (st, Nothing)

    -- TODO Implement
    storeLookupDiffIds (MemStorage ref) ik =
        atomicModifyIORef' ref get
      where
        get st = (st, [])

    -- TODO: Implement
    storeDeleteDiff (MemStorage ref) did =
        atomicModifyIORef' ref del
      where
        del st = (st, ())

    storeDeleteDiffs (MemStorage ref) ik =
        atomicModifyIORef' ref del
      where
        del st = let ds = memDiffs st
                     ikv = internalKeyValue ik
                     ds' = M.delete ikv ds
                     st' = st { memDiffs = ds' }
                 in (st', 0)

    storeRecordNotification (MemStorage ref) ik did = do
        t <- getCurrentTime
        atomicModifyIORef' ref (ins t)
      where
        ins t st =
            let (entity, key) = internalKeyValue ik
                note = Notification t (T.pack entity) key did
                st' = st { memNotes = (memNotes st) ++ [note] }
            in (st', ())

    storeFetchNotifications (MemStorage ref) limit =
        atomicModifyIORef' ref get
      where
        get st =
            let (del, keep) = splitAt limit $ memNotes st
                remaining = length keep
                st' = st { memNotes = keep }
            in (st', (remaining, del))
