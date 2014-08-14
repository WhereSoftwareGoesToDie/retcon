--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.

{-# LANGUAGE OverloadedStrings #-}

module TestScaffold where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as H
import qualified Data.Map as M
import System.Directory
import System.FilePath
import System.Process
import Test.Hspec

import Retcon.DataSource
import Retcon.DataSource.JsonDirectory
import Retcon.Diff
import Retcon.Document

import TreeHelpers

-- | Preload target directory
preloadTargetDir :: FilePath -> FilePath -> IO ()
preloadTargetDir src dst = do
    srcOK <- doesDirectoryExist src
    if srcOK
        then do
            createDirectoryIfMissing True dst
            _ <- system ("cp -r " ++ src ++ " " ++ dst)
            return ()
        else error ("Can not find source directory " ++ src)
