--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

-- | Description: Abstract storage for retcon operational data.
--
-- Retcon maintains quite a lot of operational data. This module defines an
-- interface which can be implemented against various data storage backends.
--
-- The library includes type implementations of this interface in the
-- 'Retcon.Store.PostgreSQL' and 'Retcon.Store.Memory' modules.
module Retcon.Store where

import Retcon.DataSource
import Retcon.Diff
import Retcon.Document
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
    initialiseStorage :: RetconOptions
                      -> IO s

    -- | Allocate and return a new 'InternalKey'.
    createInternalKey :: forall entity. (RetconEntity entity)
                      => s
                      -> IO (InternalKey entity)

    -- | Find the 'InternalKey' associated with a 'ForeignKey'.
    lookupInternalKey :: (RetconDataSource e d)
                      => s
                      -> ForeignKey e d
                      -> IO (Maybe (InternalKey e))

    -- | Delete an 'InternalKey' and any associated resources.
    deleteInternalKey :: (RetconEntity entity)
                      => s
                      -> InternalKey entity
                      -> IO ()

    -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
    recordForeignKey :: (RetconDataSource e d)
                     => s
                     -> InternalKey e
                     -> ForeignKey e d
                     -> IO ()

    -- | Delete a 'ForeignKey'.
    deleteForeignKey :: (RetconDataSource e d)
                     => s
                     -> ForeignKey e d
                     -> IO ()

    -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
    deleteForeignKeys :: (RetconEntity e)
                     => s
                     -> InternalKey e
                     -> IO ()

    -- | Find the 'ForeignKey' corresponding to an 'InternalKey' in a particular
    -- data source.
    lookupForeignKey :: (RetconDataSource e d)
                     => s
                     -> InternalKey e
                     -> IO (Maybe (ForeignKey e d))

    -- | Record the initial 'Document' associated with an 'InternalKey'.
    recordInitialDocument :: (RetconEntity e)
                          => s
                          -> InternalKey e
                          -> Document
                          -> IO ()

    -- | Lookup the initial 'Document', if any, associated with an 'InternalKey'.
    lookupInitialDocument :: (RetconEntity e)
                          => s
                          -> InternalKey e
                          -> IO (Maybe Document)

    -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
    deleteInitialDocument :: (RetconEntity e)
                          => s
                          -> InternalKey e
                          -> IO ()

    -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
    -- processed 'InternalKey'.
    recordDiffs :: (RetconEntity e)
                => s
                -> InternalKey e
                -> (Diff l, [Diff l])
                -> IO ()

    -- | Delete the 'Diff's associated with an 'InternalKey'.
    deleteDiffs :: (RetconEntity e)
                => s
                -> InternalKey e
                -> IO Int

