--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: PostgreSQL storage for operational data.
--
-- Retcon maintains quite a lot of operational data. This implements the
-- operational data storage interface using a PostgreSQL database.
module Retcon.Store.PostgreSQL where

import Database.PostgreSQL.Simple

import Retcon.Store

-- | Wrapper around PostgreSQL connection for backend storage.
newtype PGStorage = PGStorage { unWrapConnection :: Connection }

instance RetconStore PGStorage where

    initialiseStorage = return undefined

    createInternalKey conn = return undefined

    lookupInternalKey conn fk = return Nothing

    deleteInternalKey conn ik = return ()

    recordForeignKey conn ik fk = return ()

    deleteForeignKey conn fk = return ()

    lookupForeignKey conn ik = return Nothing
    
    recordInitialDocument conn ik doc = return ()

    lookupInitialDocument conn ik = return Nothing

    deleteInitialDocument conn ik = return ()

    recordDiffs conn ik (d, ds) = return ()

    deleteDiffs conn ik = return 0

