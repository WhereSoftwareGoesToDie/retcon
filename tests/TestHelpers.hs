--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.

{-# LANGUAGE OverloadedStrings #-}

module TestHelpers where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as M
import System.Directory
import System.FilePath
import Test.Hspec

import Retcon.Diff
import Retcon.Document
import Retcon.JsonDirectory

import TreeHelpers

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

-- | Load a 'Document' from a JSON file, raising an exception if it fails.
testLoad' :: FilePath -> IO Document
testLoad' n = testLoad n >>= return . maybe (error $ "Couldn't load " ++ n) id

-- | Load a 'Document' from a JSON file using the JsonDirectory source.
testReadJsonDir :: FilePath -> ForeignKey -> IO (Either DataSourceError Document)
testReadJsonDir fp fk = getJsonDirDocument fp fk >>= return

-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

-- | The aeson AST encoding of a test document.
testJSON :: Value
testJSON = Object $ H.fromList
    [ ("name", String "Thomas Sutton")
    , ("age", String "30")
    , ("address", Object $ H.fromList
        [ ("company", String "Anchor")
        , ("street", String "Level 11 / 201 Elizabeth Street")
        , ("locality", String "Sydney")
        ]
      )
    ]

-- | A sample 'Diff' for use in testing diff operations.
testDiff :: Diff (Int, String)
testDiff = Diff (1, "hello")
    [ InsertOp (2, "never") ["name"] "Thomas Two"
    , InsertOp (3, "gonna") ["name"] "Thomas Three"
    , InsertOp (4, "give") ["name"] "Thomas Four"
    , InsertOp (5, "you") ["name"] "Thomas Five"
    , InsertOp (6, "up") ["name"] "Thomas Six"
    ]

