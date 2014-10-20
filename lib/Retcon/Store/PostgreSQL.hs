--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections #-}

-- | Description: PostgreSQL storage for operational data.
--
-- Retcon maintains quite a lot of operational data. This implements the
-- operational data storage interface using a PostgreSQL database.
module Retcon.Store.PostgreSQL (PGStorage(..), prepareConfig) where

import Control.Lens.Operators
import Control.Monad
import Data.Aeson
import Data.List
import Data.Map.Strict (Map)
import Data.Monoid
import Data.Proxy
import Data.String
import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.TypeLits
import System.Directory
import Text.Trifecta hiding (Success, token)
import qualified Text.Trifecta as P

import Retcon.Core
import Retcon.Diff
import Retcon.Options

import Utility.Configuration

-- | A persistent, PostgreSQL storage backend for Retcon.
newtype PGStorage = PGStore { unWrapConnection :: Connection }

joinSQL :: IsString x => [String] -> x
joinSQL = fromString . intercalate ";"

-- | Persistent PostgreSQL-backed data storage.
instance RetconStore PGStorage where

    storeInitialise opts = do
        conn <- connectPostgreSQL (opts ^. optDB)
        return . PGStore $ conn

    storeFinalise (PGStore conn) =
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
        let execute' sql = execute conn sql (internalKeyValue ik)
        d1 <- execute' "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
        d2 <- execute' "DELETE FROM retcon_diff WHERE entity = ? AND id = ?"
        d3 <- execute' "DELETE FROM retcon_fk WHERE entity = ? AND id = ?"
        d4 <- execute' "DELETE FROM retcon WHERE entity = ? AND id = ?"
        return . sum . map fromIntegral $ [d1, d2, d3, d4]

    storeRecordForeignKey (PGStore conn) ik fk = do
        let (entity, source, fid) = foreignKeyValue fk
        let (_, iid) = internalKeyValue ik
        let values = (entity, iid, source, fid)
        let sql = "INSERT INTO retcon_fk (entity, id, source, fk) VALUES (?, ?, ?, ?)"
        void $ execute conn sql values

    storeDeleteForeignKey (PGStore conn) fk = do
        let sql = "DELETE FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ?"
        d <- execute conn sql $ foreignKeyValue fk
        return $ fromIntegral  d

    storeDeleteForeignKeys (PGStore conn) ik = do
        let sql = "DELETE FROM retcon_fk WHERE entity = ? AND id = ?"
        d <- execute conn sql $ internalKeyValue ik
        return $ fromIntegral d

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
        let sql = joinSQL [ "BEGIN"
                          , "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
                          , "INSERT INTO retcon_initial (id, entity, document) VALUES (?, ?, ?)"
                          , "COMMIT"
                          ]
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
        d <- execute conn sql $ internalKeyValue ik
        return $ fromIntegral d

    storeRecordDiffs (PGStore conn) ik (d, ds) = do
        -- Relabel the diffs with () instead of the arbitrary, possibly
        -- unserialisable, labels.
        did <- storeOneDiff conn ik (void d)
        let ops = map ((did,) . toJSON) . concatMap (_diffChanges . void) $ ds
        void $ executeMany conn "INSERT INTO retcon_diff_conflicts (diff_id, content) VALUES (?, ?)" ops
        return did

    storeLookupDiff (PGStore conn) diff_id = do
        let oid = Only diff_id

        -- Load the merged diff.
        diff <- query conn "SELECT content FROM retcon_diff WHERE diff_gd = ?" oid
        let diff' = map (fromJSON . fromOnly) diff

        -- Load the conflicting fragments.
        (conflicts) <- query conn "SELECT content FROM retcon_diff WHERE diff_id = ?" oid
        let conflicts' = map fromSuccess . filter isSuccess . map (fromJSON . fromOnly) $ conflicts

        return $ case diff' of
            Success d:_ -> Just (d, conflicts')
            _           -> Nothing
      where
        fromSuccess (Success a) = a
        fromSuccess _ = error "fromSuccess: Cannot unwrap not-a-success."
        isSuccess (Success _) = True
        isSuccess _ = False

    storeLookupDiffIds (PGStore conn) ik = do
        r <- query conn "SELECT diff_id FROM retcon_diff WHERE entity = ? AND id = ?" $ internalKeyValue ik
        return . map fromOnly $ r

    storeDeleteDiff (PGStore conn) diff_id = do
        d <- execute conn "DELETE FROM retcon_diff WHERE diff_id = ?" (Only diff_id)
        return $ fromIntegral  d

    storeDeleteDiffs (PGStore conn) ik = do
        let execute' sql = execute conn sql (internalKeyValue ik)
        d1 <- execute' "DELETE FROM retcon_diff_conflicts WHERE diff_id IN (SELECT diff_id FROM retcon_diff WHERE entity = ? AND id = ?)"
        d2 <- execute' "DELETE FROM retcon_notifications WHERE entity = ? AND id = ?"
        d3 <- execute' "DELETE FROM retcon_diff WHERE entity = ? AND id = ?"
        -- TODO: Count the number of items deleted and return that here.
        return . sum . map fromIntegral $ [d1, d2, d3]

    storeRecordNotification (PGStore conn) ik did = do
        let (entity, eid) = internalKeyValue ik
        void $ execute conn sql (entity, eid, did)
      where
        sql = "INSERT INTO retcon_notifications (entity, id, diff_id) VALUES (?, ?, ?)"

    storeFetchNotifications (PGStore conn) limit = do
        (ids :: [Only Int]) <- query conn sqlI (Only limit)
        let ids' = map fromOnly ids
        let least = minimum ids'
        let greatest = maximum ids'
        notifications <- query conn sqlN (least, greatest)
        [Only remaining] <- query conn sqlD (least, greatest)
        return (remaining, notifications)
      where
        sqlI = "SELECT id FROM retcon_notifications ORDER BY created ASC LIMIT ?"
        sqlN = "SELECT created, entity, key, diff_id FROM retcon_notifications WHERE id >= ? AND id <= ?"
        sqlD = "DELETE FROM retcon_notifications WHERE id >= ? AND id <= ?; SELECT count(*) FROM retcon_notifications"

-- | Record a single 'Diff' object into the database, returning the ID of that
-- new diff.
storeOneDiff
    :: (RetconEntity entity)
    => Connection
    -> InternalKey entity
    -> Diff ()
    -> IO Int
storeOneDiff conn ik d = do
    let (ikentity, ikid) = internalKeyValue ik
    [Only did] <- query conn q (ikentity, ikid, encode d)
    return did
  where
    q = "INSERT INTO retcon_diff (entity, id, content) VALUES (?, ?, ?) RETURNING diff_id"


-- | Load the parameters from the path specified in the options.
prepareConfig
    :: (RetconOptions, [Text])
    -> [entity]
    -> IO (RetconConfig entity RWToken)
prepareConfig (opt, event) entities = do
    params <- maybe (return mempty) readParams $ opt ^. optParams
    store :: PGStorage <- storeInitialise opt
    return $ RetconConfig
        (opt ^. optVerbose)
        (opt ^. optLogging)
        (token store)
        params
        event
        entities
  where
    readParams :: FilePath -> IO (Map (Text, Text) (Map Text Text))
    readParams path = do
        exists <- doesFileExist path
        results <- if exists
            then parseFromFileEx configParser path
            else error $ "specified config file doesn not exist: \"" ++ path ++ "\""
        case results of
            P.Success results' -> return $ convertConfig results'
            P.Failure failure -> error $ show failure
