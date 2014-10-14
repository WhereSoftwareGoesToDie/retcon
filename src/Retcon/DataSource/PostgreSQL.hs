{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Retcon.DataSource.PostgreSQL (
    getPgDocument,
    setPgDocument,
    deletePgDocument,
    DBName(..),
    pgConnStr,
    DataSourceError,
    ForeignKey
    ) where

import Control.Applicative
import qualified Control.Exception as Ex
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.String

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Typeable
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Types as PGT

import Retcon.DataSource
import Retcon.Document


-- | A wrapper for a string that represents a PostgreSQL database name. This is
-- used to escape for various things.
newtype DBName = DBName { unDBName :: String }
  deriving IsString

-- | Convert a 'DBName' to a database connection string for postgres. i.e.
-- "dbname='magical_db'"
pgConnStr :: DBName -> ByteString
pgConnStr (DBName db) = "dbname='" <> BS.pack db <> "'"

data DataSourceError = DataSourceError String deriving (Show, Typeable)
instance Ex.Exception DataSourceError

-- | Define FromRow for Documents. Using Maybe so we can catch whether or not it parses valid JSON.
instance FromRow (Maybe Document) where
    fromRow = decode <$> field

-- | API function getDocument
getPgDocument :: DBName -> ForeignKey entity source -> IO (Either DataSourceError Document)
getPgDocument db fk = Ex.bracket (PG.connectPostgreSQL $ pgConnStr db) PG.close run
    where
        run conn = do
            results <- PG.query conn selectQ (PGT.Only $ unForeignKey fk)
            case results of
                [] -> return $ Left (DataSourceError "Nothing to get")
                (PGT.Only fdoc:_) -> case fromJSON fdoc of
                    Error msg   -> return $ Left (DataSourceError msg)
                    Success doc -> return $ Right doc
        selectQ = "SELECT data FROM retcon WHERE id = ?" :: PGT.Query

-- | API function setDocument
setPgDocument :: forall entity source. RetconDataSource entity source
              => DBName
              -> Document
              -> Maybe (ForeignKey entity source)
              -> IO (Either DataSourceError (Maybe (ForeignKey entity source)))
setPgDocument db doc mfk = Ex.bracket (PG.connectPostgreSQL $ pgConnStr db) PG.close run
    where
        run conn =
            case mfk of
                Nothing -> do
                    newFK <- PG.returning conn insertQ [PGT.Only enc]
                    case newFK of
                        []                        -> return $ Left (DataSourceError "Could not insert")
                        (PGT.Only (x :: Int) : _) -> return $ Right $ Just (ForeignKey (show x) :: ForeignKey entity source)
                Just fk -> do
                    updated <- PG.execute conn updateQ (enc, unForeignKey fk)
                    case updated of
                        0 -> return $ Left (DataSourceError "Nothing to update")
                        _ -> return $ Right $ Just fk
        enc = encode doc
        insertQ = "INSERT INTO retcon (data) VALUES (?) RETURNING id"
        updateQ = "UPDATE retcon SET data = ? WHERE id = ?"

-- | API function deleteDocument
deletePgDocument :: DBName -> ForeignKey entity source -> IO (Either DataSourceError ())
deletePgDocument db fk = Ex.bracket (PG.connectPostgreSQL $ pgConnStr db) PG.close run
    where
        run conn = do
            deleted <- PG.execute conn deleteQ (PGT.Only $ unForeignKey fk)
            case deleted of
                0 -> return $ Left (DataSourceError "Nothing to delete")
                _ -> return $ Right ()
        deleteQ = "DELETE FROM retcon WHERE id = ?"
