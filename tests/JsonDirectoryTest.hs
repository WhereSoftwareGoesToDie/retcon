--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import System.Directory
import System.FilePath

import TestHelpers

suite :: Spec
suite = do
  describe "JSON directory marshalling" $ do
    it "can load 01-diff-source" $ do
      f <- fp
      test1 <- testReadJsonDir f "01-diff-source"
      either (error "Could not load file") (const pass) test1

    it "can load 01-diff-target" $ do
      f <- fp
      test1 <- testReadJsonDir f "01-diff-target"
      either (error "Could not load file") (const pass) test1

fp :: IO FilePath
fp = do
  cwd <- getCurrentDirectory
  return $ joinPath [cwd, "tests", "data"]

main :: IO ()
main = hspec suite

