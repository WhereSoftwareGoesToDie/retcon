--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.

{-# LANGUAGE OverloadedStrings #-}

module TestHelpers where

import Control.Applicative
import Control.Monad.IO.Class
import System.FilePath.Posix

import Retcon.DataSource.JsonDirectory
import Retcon.Diff
import Retcon.Document
import System.Directory

-- | Explicitly pass a test
pass :: Monad m => m ()
pass = return ()

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
testJSONFilePath = liftIO $
    (</> "tests" </> "data" </> "json") <$> getCurrentDirectory

testLoad :: FilePath -> IO Document
testLoad file = do
    path <- (</> file) <$> testJSONFilePath
    loadDocument path
