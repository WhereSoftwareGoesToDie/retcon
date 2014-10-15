{-# LANGUAGE DeriveDataTypeable #-}

module Retcon.DataSource.JsonDirectory (
    getJSONDir,
    setJSONDir,
    deleteJSONDir,
    JSONDirectoryError,
    ForeignKey,
    loadDocument
) where

import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import Data.Typeable

import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import System.Random

import Retcon.DataSource
import Retcon.Document

data JSONDirectoryError = JSONDirectoryError String deriving (Show, Typeable)
instance Exception JSONDirectoryError

-- | Read a 'Document' from the given JSON directory using information from the
-- 'ForeignKey' provided.
getJSONDir
    :: (MonadIO m, RetconDataSource entity source)
    => FilePath
    -> ForeignKey entity source
    -> m Document
getJSONDir dir fk = loadDocument (buildPath dir fk)

-- | Write a 'Document' to the given JSON directory using information from the
-- 'ForeignKey' provided.
setJSONDir
    :: (MonadIO m, RetconDataSource entity source)
    => FilePath
    -> Document
    -> Maybe (ForeignKey entity source)
    -> m (ForeignKey entity source)
setJSONDir dir doc m_fk = liftIO $ do
    fk <- maybe (newFK dir) return m_fk
    LBS.writeFile (buildPath dir fk) (encode doc)
    return fk

-- | Unlink the underlying JSON file corresponding to the directory and
-- 'ForeignKey' provided.
deleteJSONDir
    :: (MonadIO m, RetconDataSource entity source)
    => FilePath
    -> ForeignKey entity source
    -> m ()
deleteJSONDir dir fk = liftIO $
    removeFile (buildPath dir fk)

-- | Generate a new foreign key using chars a-z.
newFK
    :: RetconDataSource entity source
    => FilePath
    -> IO (ForeignKey entity source)
newFK dir = do
    k <- ForeignKey . take 64 . randomRs ('a', 'z') <$> newStdGen
    exists <- fileExist (buildPath dir k)
    if exists
        then newFK dir
        else return k

-- | Construct a path to a JSON file given the base directory and a
-- 'ForeignKey'.
buildPath
    :: RetconDataSource entity source
    => FilePath
    -> ForeignKey entity source
    -> FilePath
buildPath base fk =
    let (entity, source, key) = foreignKeyValue fk
    in  base </> entity </> source </> key ++ ".json"

-- | Decode a 'Document' from the JSON file at the given 'FilePath'
loadDocument
    :: MonadIO m
    => FilePath
    -> m Document
loadDocument fp = liftIO $
    eitherDecode <$> LBS.readFile fp 
    >>= either (throwIO . JSONDirectoryError) return
