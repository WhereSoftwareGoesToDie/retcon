{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy  as BS
import           Data.Maybe
import           System.Directory
import           System.Exit
import           System.FilePath

import Retcon.Diff
import Retcon.Document

-- | A sample 'Diff' to re-label.
testDiff :: Diff (Int, String)
testDiff = Diff (1, "hello")
    [ InsertOp (2, "never") ["name"] "Thomas"
    , InsertOp (2, "gonna") ["name"] "Thomas"
    , InsertOp (2, "give") ["name"] "Thomas"
    , InsertOp (2, "you") ["name"] "Thomas"
    , InsertOp (2, "up") ["name"] "Thomas"
    ]

-- | Get the path to a test data file.
testDataFile :: FilePath -> IO FilePath
testDataFile file = do
    cwd <- getCurrentDirectory
    return $ joinPath [cwd, "tests", "data", file]

-- | Load a 'Document' from a JSON file.
testLoad :: FilePath -> IO (Maybe Document)
testLoad name = do
    file <- testDataFile name
    input <- BS.readFile file
    return $ decode input

main :: IO ()
main = do
  -- Try to load a document from JSON.
  test1 <- testLoad "01-diff-source.json"
  source <- case test1 of
    Just d  -> return d
    Nothing -> do
      putStrLn "Could not load 01-diff-source.json as document."
      exitFailure

  -- Try to load another document from JSON.
  test2 <- testLoad "01-diff-target.json"
  target <- case test2 of
    Just d -> return d
    Nothing -> do
      putStrLn "Could not load 01-diff-target.json as document."
      exitFailure

  -- Try to diff the documents.
  let diff' = diff source target
  print diff'
  
  exitSuccess

