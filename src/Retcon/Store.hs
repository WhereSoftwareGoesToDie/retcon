--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}

-- | Description: Abstract storage for retcon operational data.
--
-- Retcon maintains quite a lot of operational data. This module defines an
-- interface which can be implemented against various data storage backends,
-- along with types and typeclasses which provide both a restricted and a
-- permissive interface to this.
--
-- The library includes type implementations of this interface in the
-- "Retcon.Store.PostgreSQL" and "Retcon.Store.Memory" modules.
module Retcon.Store (
    -- * Storage backends
    RetconStore(..),
    -- * API
    StoreToken(..),
    ReadableToken(..),
    WritableToken(..),
    -- * Tokens
    token,
    ROToken,
    RWToken,
    ) where

import Control.Monad.IO.Class

import Retcon.DataSource
import Retcon.Diff
import Retcon.Document
import Retcon.Monad
import Retcon.Options

-- | A storage backend for retcon operational data
--
-- In production this will interact with a PostgreSQL database, but testing and
-- demonstrations will likely use an in-memory or other low-dependency
-- alternative.
--
-- All operations must be implemented.
class RetconStore s where

    -- | Initialise a handle to the storage backend.
    --
    -- (E.g. connect to the database server, etc.)
    storeInitialise :: RetconOptions
                    -> IO s

    -- | Finalise a handle to the storage backend.
    --
    -- (E.g. disconnect from the database server, etc.)
    storeFinalise :: s
                  -> IO ()

    -- | Allocate and return a new 'InternalKey'.
    storeCreateInternalKey :: forall entity. (RetconEntity entity)
                           => s
                           -> IO (InternalKey entity)

    -- | Find the 'InternalKey' associated with a 'ForeignKey'.
    storeLookupInternalKey :: (RetconDataSource e d)
                           => s
                           -> ForeignKey e d
                           -> IO (Maybe (InternalKey e))

    -- | Delete an 'InternalKey' and any associated resources.
    storeDeleteInternalKey :: (RetconEntity entity)
                           => s
                           -> InternalKey entity
                           -> IO ()

    -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
    storeRecordForeignKey :: (RetconDataSource e d)
                          => s
                          -> InternalKey e
                          -> ForeignKey e d
                          -> IO ()

    -- | Delete a 'ForeignKey'.
    storeDeleteForeignKey :: (RetconDataSource e d)
                          => s
                          -> ForeignKey e d
                          -> IO ()

    -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
    storeDeleteForeignKeys :: (RetconEntity e)
                           => s
                           -> InternalKey e
                           -> IO ()

    -- | Find the 'ForeignKey' corresponding to an 'InternalKey' in a particular
    -- data source.
    storeLookupForeignKey :: (RetconDataSource e d)
                          => s
                          -> InternalKey e
                          -> IO (Maybe (ForeignKey e d))

    -- | Record the initial 'Document' associated with an 'InternalKey'.
    storeRecordInitialDocument :: (RetconEntity e)
                               => s
                               -> InternalKey e
                               -> Document
                               -> IO ()

    -- | Lookup the initial 'Document', if any, associated with an 'InternalKey'.
    storeLookupInitialDocument :: (RetconEntity e)
                               => s
                               -> InternalKey e
                               -> IO (Maybe Document)

    -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
    storeDeleteInitialDocument :: (RetconEntity e)
                               => s
                               -> InternalKey e
                               -> IO ()

    -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
    -- processed 'InternalKey'.
    storeRecordDiffs :: (RetconEntity e)
                     => s
                     -> InternalKey e
                     -> (Diff l, [Diff l])
                     -> IO ()

    -- | Delete the 'Diff's associated with an 'InternalKey'.
    storeDeleteDiffs :: (RetconEntity e)
                     => s
                     -> InternalKey e
                     -> IO Int

-- * Tokens

-- $ Tokens wrap storage backend values and expose particular subsets of the
-- complete storage interface to client code.

-- | Wrap a storage backend value in a token.
token :: (RetconStore s)
      => s
      -> RWToken
token = RWToken

-- | Storage tokens expose an APIs to the underlying storage backend.
--
-- Each token will carry instances of one or more other typeclasses
-- ('ReadableToken', 'WritableToken') which define the operations permitted by
-- that token type.
class StoreToken s where
    -- | Restrict a token to be read-only.
    restrictToken :: s -> ROToken

-- | Storage tokens which support reading operations.
class StoreToken s => ReadableToken s where
    -- | Find the 'InternalKey' associated with a 'ForeignKey'.
    lookupInternalKey :: (RetconDataSource e d)
                      => s
                      -> ForeignKey e d
                      -> RetconMonad s l (Maybe (InternalKey e))

    -- | Find the 'ForeignKey' corresponding to an 'InternalKey' in a particular
    -- data source.
    lookupForeignKey :: (RetconDataSource e d)
                     => s
                     -> InternalKey e
                     -> RetconMonad s l (Maybe (ForeignKey e d))

    -- | Lookup the initial 'Document', if any, associated with an 'InternalKey'.
    lookupInitialDocument :: (RetconEntity e)
                          => s
                          -> InternalKey e
                          -> RetconMonad s l (Maybe Document)

-- | Storage tokens which support writing operations.
class StoreToken s => WritableToken s where
    -- | Allocate and return a new 'InternalKey'.
    createInternalKey :: (RetconEntity entity)
                      => s
                      -> RetconMonad s l (InternalKey entity)

    -- | Delete an 'InternalKey' and any associated resources.
    deleteInternalKey :: (RetconEntity entity)
                      => s
                      -> InternalKey entity
                      -> RetconMonad s l ()

    -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
    recordForeignKey :: (RetconDataSource e d)
                     => s
                     -> InternalKey e
                     -> ForeignKey e d
                     -> RetconMonad s l ()

    -- | Delete a 'ForeignKey'.
    deleteForeignKey :: (RetconDataSource e d)
                     => s
                     -> ForeignKey e d
                     -> RetconMonad s l ()

    -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
    deleteForeignKeys :: (RetconEntity e)
                      => s
                      -> InternalKey e
                      -> RetconMonad s l ()

    -- | Record the initial 'Document' associated with an 'InternalKey'.
    recordInitialDocument :: (RetconEntity e)
                          => s
                          -> InternalKey e
                          -> Document
                          -> RetconMonad s l ()

    -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
    deleteInitialDocument :: (RetconEntity e)
                          => s
                          -> InternalKey e
                          -> RetconMonad s l ()

    -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
    -- processed 'InternalKey'.
    recordDiffs :: (RetconEntity e)
                => s
                -> InternalKey e
                -> (Diff l, [Diff l])
                -> RetconMonad s l ()

    -- | Delete the 'Diff's associated with an 'InternalKey'.
    deleteDiffs :: (RetconEntity e)
                => s
                -> InternalKey e
                -> RetconMonad s l Int

-- | A token exposing only the 'ReadableToken' API.
data ROToken = forall s. RetconStore s => ROToken s

instance StoreToken ROToken where
    restrictToken = id

instance ReadableToken ROToken where
    lookupInternalKey (ROToken store) = liftIO . storeLookupInternalKey store

    lookupForeignKey (ROToken store) = liftIO . storeLookupForeignKey store
    
    lookupInitialDocument (ROToken store) = liftIO . storeLookupInitialDocument store

-- | A token exposing both the 'ReadableToken' and 'WritableToken' APIs.
data RWToken = forall s. RetconStore s => RWToken s

instance StoreToken RWToken where
    restrictToken (RWToken st) = ROToken st

instance ReadableToken RWToken where
    lookupInternalKey (RWToken store) = liftIO . storeLookupInternalKey store

    lookupForeignKey (RWToken store) = liftIO . storeLookupForeignKey store
    
    lookupInitialDocument (RWToken store) = liftIO . storeLookupInitialDocument store

instance WritableToken RWToken where
    createInternalKey (RWToken store) = liftIO $ storeCreateInternalKey store

    deleteInternalKey (RWToken store) = liftIO . storeDeleteInternalKey store

    recordForeignKey (RWToken store) ik fk = liftIO $ storeRecordForeignKey store ik fk

    deleteForeignKey (RWToken store) = liftIO . storeDeleteForeignKey store

    deleteForeignKeys (RWToken store) = liftIO . storeDeleteForeignKeys store

    recordInitialDocument (RWToken store) ik = liftIO . storeRecordInitialDocument store ik

    deleteInitialDocument (RWToken store) = liftIO . storeDeleteInitialDocument store

    recordDiffs (RWToken store) ik = liftIO . storeRecordDiffs store ik

    deleteDiffs (RWToken store) = liftIO . storeDeleteDiffs store
