{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Retcon.DataSource.PostgreSQL (
    getPgDocument,
    setPgDocument,
    deletePgDocument,
    DataSourceError,
    ForeignKey
    ) where

import Control.Applicative
import qualified Control.Exception as Ex
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

import Data.Aeson
import Data.Maybe
import Data.Typeable
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.FromRow
import qualified Database.PostgreSQL.Simple.Types as PGT

import Retcon.DataSource
import Retcon.Document

data DataSourceError = DataSourceError String deriving (Show, Typeable)
instance Ex.Exception DataSourceError

-- | Define FromRow for Documents. Using Maybe so we can catch whether or not it parses valid JSON.
instance FromRow (Maybe Document) where
    fromRow = decode <$> field

-- | API function getDocument
getPgDocument :: BS.ByteString -> ForeignKey entity source -> IO (Either DataSourceError Document)
getPgDocument connString fk = Ex.bracket (PG.connectPostgreSQL connString) PG.close run
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
              => BS.ByteString
              -> Document
              -> Maybe (ForeignKey entity source)
              -> IO (Either DataSourceError (Maybe (ForeignKey entity source)))
setPgDocument connString doc mfk = Ex.bracket (PG.connectPostgreSQL connString) PG.close run
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
deletePgDocument :: BS.ByteString -> ForeignKey entity source -> IO (Either DataSourceError ())
deletePgDocument connString fk = Ex.bracket (PG.connectPostgreSQL connString) PG.close run
    where
        run conn = do
            deleted <- PG.execute conn deleteQ (PGT.Only $ unForeignKey fk)
            case deleted of
                0 -> return $ Left (DataSourceError "Nothing to delete")
                _ -> return $ Right ()
        deleteQ = "DELETE FROM retcon WHERE id = ?"
