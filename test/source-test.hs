{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson

import Synchronise

source :: DataSource
source = DataSource
    { entityName = "entity"
    , sourceName = "source"
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
