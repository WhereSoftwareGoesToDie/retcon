--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}


-- | XXX TODO
-- This test has been stripped to make way for a nicer one. It is currently
-- non-functional and will always pass.
module Main where

import Test.Hspec


import System.Directory
import System.FilePath
import System.Process

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
    describe "reading" $ do
        it "reads good file" $ pending 
        it "does not read bad file" $ pending 

    describe "writing" $ do
        it "writes new file" $ pending
        it "overwrites existing file" $ pending

    describe "deletion" $
        it "deletes" $ pending

main :: IO ()
main = do
    createDirectoryIfMissing True targetFp
    hspec suite

-- | Preload target directory
--
-- TODO: Burn this before it hurts someone
preloadTargetDir :: FilePath -> FilePath -> IO ()
preloadTargetDir src dst = do
    srcOK <- doesDirectoryExist src
    if srcOK
        then do
            createDirectoryIfMissing True dst
            _ <- system ("cp -r " ++ src ++ " " ++ dst)
            return ()
        else error ("Can not find source directory " ++ src)
