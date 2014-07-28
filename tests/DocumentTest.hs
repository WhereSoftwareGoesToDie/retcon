{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import qualified Data.HashMap.Lazy as H
import qualified Data.Map          as M
import           Data.Text         (Text)
import           System.Exit
import           Test.Hspec

import Retcon.Document

import TestHelpers

-- | A sample retcon 'Document' value.
testDocument :: Document
testDocument = Document $ M.fromList
  [ ("name", Value "Thomas Sutton")
  , ("age", Value "30")
  , ("address", Subdocument $ Document $ M.fromList
      [ ("company", Value "Anchor")
      , ("street", Value "Level 11 / 201 Elizabeth Street")
      , ("locality", Value "Sydney")
      ])
  ]

-- | A sample aeson 'Value' AST encoding the 'testDocument' document
-- above.
testJSON :: Value
testJSON = Object $ H.fromList
    [ ("name", String "Thomas Sutton")
    , ("age", String "30")
    , ("address", Object $ H.fromList
        [ ("company", String "Anchor")
        , ("street", String "Level 11 / 201 Elizabeth Street")
        , ("locality", String "Sydney")
        ]
      )
    ]

suite :: Spec
suite = do
  describe "JSON marhsalling" $ do
    it "roundtrip should be idempotent" $ do
      let doc' = (decode $ encode testJSON)
      doc' `shouldBe` (Just testDocument)

main :: IO ()
main = hspec suite

lol = do
  -- Try to load a document from JSON.
  test1 <- testLoad "01-diff-source.json"
  source <- case test1 of
    Just d  -> return d
    Nothing -> do
      putStrLn "Could not load 01-diff-source.json as document."
      exitFailure

  -- Try to load another document from JSON.
  test2 <- testLoad "01-diff-target.json"
  target <- case test2 of
    Just d -> return d
    Nothing -> do
      putStrLn "Could not load 01-diff-target.json as document."
      exitFailure

  exitSuccess

