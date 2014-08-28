--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

-- | Description: PostgreSQL storage for operational data.
--
-- Retcon maintains quite a lot of operational data. This implements the
-- operational data storage interface using a PostgreSQL database.
module Retcon.Store.PostgreSQL where

import Data.Aeson
import Database.PostgreSQL.Simple
import Data.Proxy
import GHC.TypeLits

import Retcon.DataSource
import Retcon.Store
import Retcon.Options

-- | Wrapper around PostgreSQL connection for backend storage.
newtype PGStorage = PGStore { unWrapConnection :: Connection }

instance RetconStore PGStorage where

    initialiseStorage opts = do
        conn <- connectPostgreSQL . optDB $ opts
        return . PGStore $ conn

    -- | Create a new 'InternalKey' by inserting a row in the database and
    -- using the allocated ID as the new key.
    createInternalKey :: forall entity. (RetconEntity entity)
                      => PGStorage
                      -> IO (InternalKey entity)
    createInternalKey (PGStore conn) = do
        let entity = symbolVal (Proxy :: Proxy entity)
        res <- query conn "INSERT INTO retcon (entity) VALUES (?) RETURNING id" (Only entity)
        case res of
            [] -> error "Could not create new internal key"
            (Only key:_) -> return $ InternalKey key

    lookupInternalKey (PGStore conn) fk = do
        results <- query conn "SELECT id FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ? LIMIT 1" $ foreignKeyValue fk
        case results of
            Only key:_ -> return $ Just (InternalKey key)
            []         -> return Nothing

    deleteInternalKey (PGStore conn) ik = do
        let sql = "DELETE FROM retcon WHERE entity = ? AND id = ?"
        _ <- execute conn sql $ internalKeyValue ik
        return ()

    recordForeignKey (PGStore conn) ik fk = do
        let (entity, source, fid) = foreignKeyValue fk
        let (_, iid) = internalKeyValue ik
        let values = (entity, iid, source, fid)
        let sql = "INSERT INTO retcon_fk (entity, id, source, fk) VALUES (?, ?, ?, ?)"
        _ <- execute conn sql values
        return ()

    deleteForeignKey (PGStore conn) fk = do
        let sql = "DELETE FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ?"
        _ <- execute conn sql $ foreignKeyValue fk
        return ()

    deleteForeignKeys (PGStore conn) ik = do
        let sql = "DELETE FROM retcon_fk WHERE entity = ? AND id = ?"
        _ <- execute conn sql $ internalKeyValue ik
        return ()

    lookupForeignKey :: forall entity source. (RetconDataSource entity source)
                     => PGStorage
                     -> InternalKey entity
                     -> IO (Maybe (ForeignKey entity source))
    lookupForeignKey (PGStore conn) ik = do
        let source = symbolVal (Proxy :: Proxy source)
        let (entity, ik') = internalKeyValue ik
        let sql = "SELECT fk FROM retcon_fk WHERE entity = ? AND id = ? AND source = ?"
        res <- query conn sql (entity, ik', source)
        return $ case res of
            []         -> Nothing
            Only fk':_ -> Just $ ForeignKey fk'

    recordInitialDocument (PGStore conn) ik doc = do
        let (entity, ik') = internalKeyValue ik
        let sql = "BEGIN; DELETE FROM retcon_initial WHERE entity = ? AND id = ?; INSERT INTO retcon_initial (id, entity, document) VALUES (?, ?, ?); COMMIT;"
        _ <- execute conn sql (entity, ik', ik', entity, toJSON doc)
        return ()

    lookupInitialDocument (PGStore conn) ik = do
        let sql = "SELECT document FROM retcon_initial WHERE entity = ? AND id = ?"
        res <- query conn sql $ internalKeyValue ik
        case res of
            []       -> return Nothing
            Only v:_ -> case fromJSON v of
                Error _     -> return Nothing
                Success doc -> return . Just $ doc

    deleteInitialDocument (PGStore conn) ik = do
        let sql = "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
        _ <- execute conn sql $ internalKeyValue ik
        return ()

    recordDiffs (PGStore conn) ik (d, ds) = do
        return ()

    deleteDiffs (PGStore conn) ik = do
        let sql1 = "DELETE FROM retcon_diff_portion WHERE entity = ? AND id = ?"
        let sql2 = "DELETE FROM retcon_diff WHERE entity = ? AND id = ?"
        _ <- execute conn sql1 $ internalKeyValue ik
        _ <- execute conn sql2 $ internalKeyValue ik
        return 0

