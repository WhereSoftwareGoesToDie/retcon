{-# LANGUAGE DeriveDataTypeable #-}

module Retcon.JsonDirectory (
	getJsonDirDocument,
	setJsonDirDocument,
	deleteJsonDirDocument,
	DataSourceError,
	ForeignKey
	) where

-- import Control.Applicative
import qualified Control.Exception as Ex
import qualified Data.ByteString.Lazy as BSL

import Data.Aeson
import Data.Typeable
import Data.Time
import Data.Time.Clock.POSIX

import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import System.Random

import Retcon.Document

-- write all actions
-- take an argument to look in
-- wrap in a newtype
-- define instance on both

--getDocument :: (RetconSource m) => ForeignKey -> m (Either DataSourceError Document)

--setDocument :: (RetconSource m) => Maybe ForeignKey -> Document -> m (Either DataSourceError (Maybe ForeignKey))

--deleteDocument :: (RetconSource m) => ForeignKey-> m (Either DataSourceError ())

type ForeignKey = String

data DataSourceError = DataSourceError String deriving (Show, Typeable)
instance Ex.Exception DataSourceError

-- | API function getDocument
getJsonDirDocument :: FilePath -> ForeignKey -> IO (Either DataSourceError Document)
getJsonDirDocument dir fk = do
	readOK <- readFileFromDocument fp
	case (readOK) of
		Left err   -> do
			return $ Left (DataSourceError "barf")
		Right fdoc -> do
			return $ Right fdoc
	where
		fp = getFkFilename dir fk

-- | API function setDocument
setJsonDirDocument :: FilePath -> Maybe ForeignKey -> Document -> IO (Either DataSourceError (Maybe ForeignKey))
setJsonDirDocument dir mfk doc = do
	case mfk of
		Nothing -> do
			nfk <- newFK dir
			writeOK <- doWrite nfk
			either throwException (\x -> return $ Right $ Just nfk) writeOK
		Just fk -> do
			writeOK <- doWrite fk
			either throwException (\x -> return $ Right $ Just fk) writeOK
	where
		doWrite k      = writeDocumentToFile dir k doc
		throwException = \ex -> return $ Left ex

-- | API function deleteDocument
deleteJsonDirDocument :: FilePath -> ForeignKey -> IO (Either DataSourceError ())
deleteJsonDirDocument dir fk = do
	exists <- fileExist fp
	case (exists) of
		False -> do
			return $ Left (DataSourceError "File does not exist")
		True  -> do
			Ex.try (removeFile fp)
	where
		fp = getFkFilename dir fk

-- | Reads retcon document from file
readFileFromDocument :: FilePath -> IO (Either String Document)
readFileFromDocument fp = do
	exists <- fileExist fp
	case (exists) of
		False -> do
			return $ Left "File does not exist"
		True  -> do
			fbs <- BSL.readFile fp
			return $ eitherDecode fbs

-- | Converts a foreign key to a filename
getFkFilename :: FilePath -> ForeignKey -> FilePath
getFkFilename dir fk = dir </> (fk ++ ".json")

-- | Generates a new foreign key using chars a-z
newFK :: FilePath -> IO ForeignKey
newFK dir = do
	s <- makeSeed
	let newKey = makeKey s
	exists <- fileExist $ tryPath newKey
	case (exists) of
		True  -> do
			newFK dir
		False -> do
			return newKey
	where
		tryPath fk = getFkFilename dir fk

-- | make key out of random seed
makeKey :: Int -> ForeignKey
makeKey s = Prelude.take 64 $ (randomRs ('a', 'z') . mkStdGen) s

-- | create random seed
makeSeed :: IO Int
makeSeed = do
	t <- getPOSIXTime
	return $ floor (t * 1000000)

-- | Writes a retcon document to a file
writeDocumentToFile :: FilePath -> ForeignKey -> Document -> IO (Either DataSourceError ())
writeDocumentToFile dir fk doc = Ex.try (BSL.writeFile fp $ encode doc)
	where
		fp = getFkFilename dir fk
