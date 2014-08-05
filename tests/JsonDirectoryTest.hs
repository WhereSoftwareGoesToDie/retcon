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
      test2 <- testReadJsonDir f "01-diff-target"
      either (error "Could not load file") (const pass) test2

  describe "JSON directory writing" $ do
    it "can write new file" $ do
      f <- fp
      preTest3 <- testReadJsonDir f "01-diff-source"
      case preTest3 of
        Left err  -> error "Should have got a document but didn't"
        Right doc -> do
          test3 <- testWriteJsonDirNew f doc
          either (error "Could not write file") (const pass) test3

    it "can write existing file" $ do
      f <- fp
      preTest4a <- testReadJsonDir f "01-diff-source"
      case preTest4a of
        Left err  -> error "Should have got a document but didn't"
        Right doc -> do
          preTest4b <- testWriteJsonDirNew f doc
          case preTest4b of
            Left err        -> error "Couldn't write document when we should have done so"
            Right (Nothing) -> error "Didn't get a ForeignKey"
            Right (Just fk) -> do
              test4 <- testWriteJsonDirExisting f fk doc
              either (error "Could not write file again") (const pass) test4

  describe "JSON directory deleting" $ do
    it "can delete existing file" $ do
      f <- fp
      preTest5a <- testReadJsonDir f "01-diff-source"
      case preTest5a of
        Left err  -> error "Should have got a document but didn't"
        Right doc -> do
          preTest5b <- testWriteJsonDirNew f doc
          case preTest5b of
            Left err        -> error "Couldn't write document when we should have done so"
            Right (Nothing) -> error "Didn't get a ForeignKey"
            Right (Just fk) -> do
              test5 <- testDeleteJsonDir f fk
              either (error "Could not write file again") (const pass) test5



fp :: IO FilePath
fp = do
  cwd <- getCurrentDirectory
  return $ joinPath [cwd, "tests", "data"]

main :: IO ()
main = hspec suite

