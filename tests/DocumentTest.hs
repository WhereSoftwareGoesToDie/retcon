{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.HashMap.Lazy as H
import           System.Exit

import Retcon.Document

testData :: Value
testData = Object $ H.fromList
    [ ("name", String "Thomas Sutton")
    , ("age", Number 30)
    , ("address", Object $ H.fromList
        [ ("company", String "Anchor")
        , ("street", String "Level 11 / 201 Elizabeth Street")
        , ("locality", String "Sydney")
        ]
      )
    ]

main :: IO ()
main = do
  let doc = (decode $ encode testData :: Maybe  Document)
  putStrLn "Test document functionality"
  print doc
  case doc of
        Nothing -> exitFailure
        Just _  -> exitSuccess

