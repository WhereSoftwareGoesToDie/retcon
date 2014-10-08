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
import qualified Data.HashMap.Lazy as H
import Control.Monad.IO.Class
import System.FilePath.Posix

import System.Directory
import Retcon.Diff
import Retcon.Document
import Retcon.DataSource.JsonDirectory

-- | Explicitly pass a test
pass :: Monad m => m ()
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

-- | Create a test path based on the CWD
testJSONFilePath :: MonadIO m => m FilePath
testJSONFilePath = liftIO $ do
    cwd <- getCurrentDirectory
    return $ cwd </> "tests" </> "data" </> "json"

testLoad :: FilePath -> IO Document
testLoad file = do
    base <- testJSONFilePath
    loadDocument $ base </> file
