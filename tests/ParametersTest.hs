
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Text.Trifecta
import Test.Hspec

import Utility.Configuration

parametersSuite :: Spec
parametersSuite = describe "Configuration parameters" $ do
    it "should parse canned data correctly" $ do
        let input = "entity.source.key=\"value\""
        let Success output = parseString configParser mempty input

        output `shouldBe` [("entity", "source", "key", "value")]

    it "should parse a canned file correctly" $ do
        Just output <- parseFromFile configParser "tests/data/parameters.conf"
        let config = convertConfig output 

        M.size config `shouldBe` 2
        M.keys config `shouldBe` [("dispatchtest", "dispatch1"),
            ("dispatchtest", "dispatch2")]
        M.lookup ("dispatchtest", "dispatch2") config `shouldBe` (Just $ M.fromList [("connstring", "dbname=database")])

main :: IO ()
main = hspec parametersSuite
