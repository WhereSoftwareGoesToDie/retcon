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
module Retcon.Store.PostgreSQL (PGStorage(..)) where

import Data.Aeson
import Database.PostgreSQL.Simple
import Data.Proxy
import GHC.TypeLits

import Retcon.DataSource
import Retcon.Store
import Retcon.Options

-- | A persistent, PostgreSQL storage backend for Retcon.
newtype PGStorage = PGStore { unWrapConnection :: Connection }

-- | Persistent PostgreSQL-backed data storage.
instance RetconStore PGStorage where

    storeInitialise opts = do
        conn <- connectPostgreSQL . optDB $ opts
        return . PGStore $ conn

    storeFinalise (PGStore conn) = do
        close conn

    -- | Create a new 'InternalKey' by inserting a row in the database and
    -- using the allocated ID as the new key.
    storeCreateInternalKey :: forall entity. (RetconEntity entity)
                      => PGStorage
                      -> IO (InternalKey entity)
    storeCreateInternalKey (PGStore conn) = do
        let entity = symbolVal (Proxy :: Proxy entity)
        res <- query conn "INSERT INTO retcon (entity) VALUES (?) RETURNING id" (Only entity)
        case res of
            [] -> error "Could not create new internal key"
            (Only key:_) -> return $ InternalKey key

    storeLookupInternalKey (PGStore conn) fk = do
        results <- query conn "SELECT id FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ? LIMIT 1" $ foreignKeyValue fk
        case results of
            Only key:_ -> return $ Just (InternalKey key)
            []         -> return Nothing

    storeDeleteInternalKey (PGStore conn) ik = do
        let sql = "DELETE FROM retcon WHERE entity = ? AND id = ?"
        _ <- execute conn sql $ internalKeyValue ik
        return ()

    storeRecordForeignKey (PGStore conn) ik fk = do
        let (entity, source, fid) = foreignKeyValue fk
        let (_, iid) = internalKeyValue ik
        let values = (entity, iid, source, fid)
        let sql = "INSERT INTO retcon_fk (entity, id, source, fk) VALUES (?, ?, ?, ?)"
        _ <- execute conn sql values
        return ()

    storeDeleteForeignKey (PGStore conn) fk = do
        let sql = "DELETE FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ?"
        _ <- execute conn sql $ foreignKeyValue fk
        return ()

    storeDeleteForeignKeys (PGStore conn) ik = do
        let sql = "DELETE FROM retcon_fk WHERE entity = ? AND id = ?"
        _ <- execute conn sql $ internalKeyValue ik
        return ()

    storeLookupForeignKey :: forall entity source. (RetconDataSource entity source)
                     => PGStorage
                     -> InternalKey entity
                     -> IO (Maybe (ForeignKey entity source))
    storeLookupForeignKey (PGStore conn) ik = do
        let source = symbolVal (Proxy :: Proxy source)
        let (entity, ik') = internalKeyValue ik
        let sql = "SELECT fk FROM retcon_fk WHERE entity = ? AND id = ? AND source = ?"
        res <- query conn sql (entity, ik', source)
        return $ case res of
            []         -> Nothing
            Only fk':_ -> Just $ ForeignKey fk'

    storeRecordInitialDocument (PGStore conn) ik doc = do
        let (entity, ik') = internalKeyValue ik
        let sql = "BEGIN; DELETE FROM retcon_initial WHERE entity = ? AND id = ?; INSERT INTO retcon_initial (id, entity, document) VALUES (?, ?, ?); COMMIT;"
        _ <- execute conn sql (entity, ik', ik', entity, toJSON doc)
        return ()

    storeLookupInitialDocument (PGStore conn) ik = do
        let sql = "SELECT document FROM retcon_initial WHERE entity = ? AND id = ?"
        res <- query conn sql $ internalKeyValue ik
        case res of
            []       -> return Nothing
            Only v:_ -> case fromJSON v of
                Error _     -> return Nothing
                Success doc -> return . Just $ doc

    storeDeleteInitialDocument (PGStore conn) ik = do
        let sql = "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
        _ <- execute conn sql $ internalKeyValue ik
        return ()

    storeRecordDiffs (PGStore conn) ik (d, ds) = do
        return ()

    storeDeleteDiffs (PGStore conn) ik = do
        let sql1 = "DELETE FROM retcon_diff_portion WHERE entity = ? AND id = ?"
        let sql2 = "DELETE FROM retcon_diff WHERE entity = ? AND id = ?"
        _ <- execute conn sql1 $ internalKeyValue ik
        _ <- execute conn sql2 $ internalKeyValue ik
        return 0
