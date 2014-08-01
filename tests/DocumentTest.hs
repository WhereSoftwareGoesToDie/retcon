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

import TestHelpers

suite :: Spec
suite = do
  describe "JSON marhsalling" $ do
    it "can load 01-diff-source.json" $ do
      test1 <- testLoad "01-diff-source.json"
      maybe (error "Could not load file") (const pass) test1

    it "can load 01-diff-target.json" $ do
      test2 <- testLoad "01-diff-target.json"
      maybe (error "Could not load file") (const pass) test2

main :: IO ()
main = hspec suite

