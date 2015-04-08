{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- * A simple synchronised datasource backed by JSON files in directories.
--
module Main where

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


data JSONOpts = JSONOpts
  { baseDir :: FilePath
  , cmmnd   :: JSONCommand }

data JSONCommand
  = Create
  | Update FilePath
  | Read   FilePath
  | Delete FilePath

pOpts :: Parser JSONOpts
pOpts
  =   JSONOpts
  <$> strArgument (metavar "BASE")
  <*> pCommand

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

main :: IO ()
main = do
  JSONOpts{..} <- execParser toplevel
  createDirectoryIfMissing True baseDir
  exit <- case cmmnd of
    Create   -> jsonCreate baseDir
    Read   f -> jsonRead   (baseDir </> f)
    Update f -> jsonWrite  (baseDir </> f)
    Delete f -> jsonDelete (baseDir </> f)
  exitWith exit
  where toplevel = info pOpts mempty

--------------------------------------------------------------------------------

-- | Creates a new JSON file under the path specified.
--   Prints the filename to `stdout`.
--
jsonCreate :: FilePath -> IO ExitCode
jsonCreate base = do
  k      <-  T.pack . take 64 . randomRs ('a', 'z')
         <$> newStdGen
  let p  = mkJSONFilename base k
  exists <- fileExist p
  if      exists
  then    jsonCreate base
  else do print k
          jsonWrite p

-- | Read a 'Document' from the given JSON directory.
--   Prints the content to `stdout`.
--
jsonRead :: FilePath -> IO ExitCode
jsonRead path = do
  doc <- loadDocument path
  handle (\(_ :: SomeException) -> return (ExitFailure 1))
         (do BL.putStr (encode doc)
             return ExitSuccess)

-- | Update a 'Document' with input from stdin.
--   Assuming the input is already in JSON and makes no attempt to check. It
--   is synchronised's repsonsiblity.
--
jsonWrite :: FilePath -> IO ExitCode
jsonWrite path = do
  doc <- B.getContents
  handle (\(_ :: SomeException) -> return (ExitFailure 1))
         (do B.writeFile path doc
             return ExitSuccess)

-- | Unlink the underlying JSON file corresponding to the directory and 'ForeignKey'.
jsonDelete :: FilePath -> IO ExitCode
jsonDelete path
  = handle (\(_ :: SomeException) -> return (ExitFailure 1))
           (do removeFile path
               return ExitSuccess)

-- | Construct a path to a JSON file.
mkJSONFilename :: FilePath -> ForeignID -> FilePath
mkJSONFilename dir f = dir </> show f </> ".json"

-- | Decode a 'Document' from the JSON file at the given 'FilePath'
loadDocument :: FilePath -> IO Document
loadDocument fp
  =  eitherDecode <$> BL.readFile fp
 >>= either (throw . ForeignError 1 . T.pack) return
