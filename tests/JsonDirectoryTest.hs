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

import Retcon.DataSource

import System.Directory
import System.FilePath

import TestHelpers
import TestScaffold

-- | get source file path
testSrc :: FilePath
testSrc = joinPath ["tests", "data"]

-- | get source file path
testDst :: FilePath
testDst = joinPath ["/tmp", "tests"]

-- | get temp source file path
sourceFp :: FilePath
sourceFp = joinPath ["/tmp", "tests", "data"]

-- | get temp target file path
targetFp :: FilePath
targetFp = joinPath ["/tmp", "tests", "test-results"]

suite :: Spec
suite = do
    describe "JSON directory marshalling" $ do
        it "can load 01-diff-source" $ do
            test1 <- testReadJsonDir sourceFp (ForeignKey "01-diff-source")
            either (error "Could not load file") (const pass) test1

        it "can load 01-diff-target" $ do
            test2 <- testReadJsonDir sourceFp (ForeignKey "01-diff-target")
            either (error "Could not load file") (const pass) test2

    describe "JSON directory writing" $ do
        it "can write new file" $ do
            preTest3 <- testReadJsonDir sourceFp (ForeignKey "01-diff-source")
            case preTest3 of
                Left err  -> error "Should have got a document but didn't"
                Right doc -> do
                    test3 <- testWriteJsonDirNew targetFp doc
                    either (error "Could not write file") (const pass) test3

        it "can write existing file" $ do
            preTest4a <- testReadJsonDir sourceFp (ForeignKey "01-diff-source")
            case preTest4a of
                Left err  -> error "Should have got a document but didn't"
                Right doc -> do
                    preTest4b <- testWriteJsonDirNew targetFp doc
                    case preTest4b of
                        Left err        -> error "Couldn't write document when we should have done so"
                        Right (Nothing) -> error "Didn't get a ForeignKey"
                        Right (Just fk) -> do
                            test4 <- testWriteJsonDirExisting targetFp doc fk
                            either (error "Could not write file again") (const pass) test4

    describe "JSON directory deleting" $ do
        it "can delete existing file" $ do
            preTest5a <- testReadJsonDir sourceFp (ForeignKey "01-diff-source")
            case preTest5a of
                Left err  -> error "Should have got a document but didn't"
                Right doc -> do
                    preTest5b <- testWriteJsonDirNew targetFp doc
                    case preTest5b of
                        Left err        -> error "Couldn't write document when we should have done so"
                        Right (Nothing) -> error "Didn't get a ForeignKey"
                        Right (Just fk) -> do
                            test5 <- testDeleteJsonDir targetFp fk
                            either (error "Could not write file again") (const pass) test5

main :: IO ()
main = do
    preloadTargetDir testSrc testDst
    createDirectoryIfMissing True targetFp
    hspec suite

