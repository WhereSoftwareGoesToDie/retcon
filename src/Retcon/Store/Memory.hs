--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: In-memory storage for operational data.
--
-- Retcon maintains quite a lot of operational data. This implements the
-- operational data storage interface using an in-memory data structure. This
-- is useful for test suites, demonstrations, etc.
module Retcon.Store.Memory (MemStorage) where

import Data.Functor
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Retcon.Document
import Retcon.Diff
import Retcon.Store

-- | Collection of in-memory data-structures to store retcon internal state.
data State = MemoryStore
    { memKeys :: Map String String
    , memInits :: Map String Document
    , memDiffs :: Map String [Diff ()]
    }

-- | An empty 'State' value.
emptyState :: State
emptyState = MemoryStore keyMap docs diffs
  where
    keyMap = M.empty
    docs = M.empty
    diffs = M.empty

-- | An ephemeral in-memory storage backend for Retcon.
newtype MemStorage = MemStorage { unwrapMemStorage :: IORef State }

-- | Ephemeral in-memory data storage.
instance RetconStore MemStorage where

    initialiseStorage _ = MemStorage <$> newIORef emptyState

    finaliseStorage (MemStorage ref) = writeIORef ref emptyState

    createInternalKey conn = return undefined

    lookupInternalKey conn fk = return Nothing

    deleteInternalKey conn ik = return ()

    recordForeignKey conn ik fk = return ()

    lookupForeignKey conn ik = return Nothing

    deleteForeignKey conn fk = return ()

    deleteForeignKeys conn ik = return ()

    recordInitialDocument conn ik doc = return ()

    lookupInitialDocument conn ik = return Nothing

    deleteInitialDocument conn ik = return ()

    recordDiffs conn ik (d, ds) = return ()

    deleteDiffs conn ik = return 0

