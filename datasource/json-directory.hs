{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- * A simple synchronised datasource backed by JSON files in directories.
--
module JSONDirectory where


import Control.Exception
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import qualified Data.Text as T
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.Posix.Files
import System.Random

import Synchronise.DataSource
import Synchronise.Document
import Synchronise.Identifier


data JSONCommand
  = Create
  | Update FilePath
  | Read   FilePath
  | Delete FilePath

pCommand :: Parser JSONCommand
pCommand
  = subparser
  (  command "create" (info pCreate (progDesc "Create a new unique JSON filename"))
  <> command "update" (info pUpdate (progDesc "Writes a document to the given JSON directory"))
  <> command "read"   (info pRead   (progDesc "Reads a document in the given JSON directory"))
  <> command "delete" (info pDelete (progDesc "Deletes a document in the given JSON directory")))
  where pCreate = pure Create
        pUpdate = Update <$> strArgument (metavar "FILEPATH")
        pRead   = Read   <$> strArgument (metavar "FILEPATH")
        pDelete = Delete <$> strArgument (metavar "FILEPATH")

jsonEntity = "json-directory"
jsonSource = "simple"

jsonNew :: FilePath -> IO ForeignID
jsonNew dir = do
  k      <-  T.pack . take 64 . randomRs ('a', 'z')
         <$> newStdGen
  exists <- fileExist (buildPath dir (ForeignKey jsonEntity jsonSource k))
  if exists
  then jsonNew dir
  else return k

jsonCreate :: IO ExitCode
jsonCreate = do
  dir <- getContents
  fi  <- jsonNew dir
  print fi
  return ExitSuccess

-- | Read a 'Document' from the given JSON directory.
--
jsonRead :: FilePath -> IO ExitCode
jsonRead filepath = do
  doc <- loadDocument filepath
  handle (\(_ :: SomeException) -> return (ExitFailure 1))
         (do BL.putStr (encode doc)
             return ExitSuccess)

-- | Update a 'Document'
jsonUpdate :: FilePath -> IO ExitCode
jsonUpdate filepath = do
  doc <- B.getContents
  handle (\(_ :: SomeException) -> return (ExitFailure 1))
         (do B.writeFile filepath doc
             return ExitSuccess)

-- | Unlink the underlying JSON file corresponding to the directory and 'ForeignKey'.
jsonDelete :: FilePath -> IO ExitCode
jsonDelete filepath
  = handle (\(_ :: SomeException) -> return (ExitFailure 1))
           (do removeFile filepath
               return ExitSuccess)

-- | Construct a path to a JSON file given the base directory and a 'ForeignKey'.
--
buildPath :: FilePath -> ForeignKey -> FilePath
buildPath base ForeignKey{..}
  = base </> (show fkEntity) </> (show fkSource) </> (show fkID) ++ ".json"

-- | Decode a 'Document' from the JSON file at the given 'FilePath'
loadDocument :: FilePath -> IO Document
loadDocument fp
  =  eitherDecode <$> BL.readFile fp
 >>= either (throw . ForeignError 1 . T.pack) return

main :: IO ()
main = do
  cmd  <- execParser toplevel
  exit <- case cmd of
    Create   -> jsonCreate
    Read   f -> jsonRead   f
    Update f -> jsonUpdate f
    Delete f -> jsonDelete f
  exitWith exit
  where toplevel = info pCommand mempty
