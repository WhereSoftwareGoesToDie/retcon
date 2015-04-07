{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- * A simple synchronised datasource backed by JSON files in directories.
--
module JSONDirectory where


import Data.Monoid
import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import System.Random
import System.Exit
import Options.Applicative
import qualified Data.Text as T

import Synchronise.Identifier
data JSONCommand
  = Create FilePath
  | Update FilePath

pCommand :: Parser JSONCommand
pCommand
  = subparser
  (  command "create" (info pCreate (progDesc "Create a new unique JSON filename"))
  <> command "update" (info pUpdate (progDesc "Writes a document to the given JSON directory")))
  where pCreate = Create <$> strArgument (metavar "FILEPATH")
        pUpdate = Update <$> strArgument (metavar "FILEPATH")

jsonEntity = "json-directory"
jsonSource = "simple"

jsonNew :: FilePath -> IO ForeignKey
jsonNew dir = do
  k      <-  ForeignKey jsonEntity jsonSource . T.pack . take 64 . randomRs ('a', 'z')
         <$> newStdGen
  exists <- fileExist (buildPath dir k)
  if exists
  then jsonNew dir
  else return k

jsonCreate :: FilePath -> IO ExitCode
jsonCreate dir = do
  fk <- jsonNew dir
  print (fkID fk)
  return ExitSuccess

-- | Construct a path to a JSON file given the base directory and a 'ForeignKey'.
--
buildPath :: FilePath -> ForeignKey -> FilePath
buildPath base ForeignKey{..}
  = base </> (show fkEntity) </> (show fkSource) </> (show fkID) ++ ".json"

main :: IO ()
main = do
  cmd  <- execParser toplevel
  exit <- case cmd of
    Create f -> jsonCreate f
  exitWith exit
  where toplevel = info pCommand mempty
