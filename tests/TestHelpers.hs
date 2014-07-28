{-# LANGUAGE OverloadedStrings #-}
module TestHelpers where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy    as H
import qualified Data.Map             as M
import           System.Directory
import           System.FilePath
import           Test.Hspec

import Retcon.Diff
import Retcon.Document

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

-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

-- | A sample retcon 'Document' value.
testDocument :: Document
testDocument = Document $ M.fromList
  [ ("name", Value "Thomas Sutton")
  , ("age", Value "30")
  , ("address", Subdocument $ Document $ M.fromList
      [ ("company", Value "Anchor")
      , ("street", Value "Level 11 / 201 Elizabeth Street")
      , ("locality", Value "Sydney")
      ])
  ]

-- | The aeson AST encoding the 'testDocument' document above.
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

