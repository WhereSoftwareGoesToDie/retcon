--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson

import Synchronise

source :: DataSource
source = DataSource
    { sourceEntity = "entity"
    , sourceName = "source"
    , sourceDescription = Nothing
    , commandCreate = "cat > entity/source/%fk.json"
    , commandRead = "cat entity/source/%fk.json"
    , commandUpdate = "cat > entity/source/%fk.json"
    , commandDelete = "rm entity/source/%fk.json"
    }

main :: IO ()
main = do
    let fk = ForeignKey "entity" "source" "123"
    doc <- runDSMonad $ get source fk
    print . toJSON $ doc
    putStrLn "LOL"
