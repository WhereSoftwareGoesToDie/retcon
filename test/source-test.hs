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

import Control.Monad
import Data.Aeson
import Data.Monoid
import Test.Hspec
import Test.HUnit

import Synchronise

source :: DataSource
source = DataSource
    { sourceEntity = "entity"
    , sourceName = "source"
    , sourceDescription = Nothing
    , commandCreate = "mkdir -p entity/source && cat > entity/source/%fk.json"
    , commandRead = "mkdir -p entity/source && cat entity/source/%fk.json"
    , commandUpdate = "mkdir -p entity/source && cat > entity/source/%fk.json"
    , commandDelete = "mkdir -p entity/source && rm entity/source/%fk.json"
    }

allDifferent :: (Eq a, Show a) => [a] -> Assertion
allDifferent [] = return ()
allDifferent (x:xs) = do
  when (x `elem` xs) $ assertFailure $ "allDifferent: found multiple occurances of " <> show x
  allDifferent xs

suite :: Spec
suite = do
    describe "DataSource" $ do
        it "can create and read out again" $ do
            let doc = Document "entity" "source" (object ["foo" .= ("bar" :: String)])
            fk' <- runDSMonad $ createDocument source doc
            fk <- case fk' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            res <- runDSMonad $ readDocument source fk
            case res of
                Left err -> assertFailure (show err)
                Right doc' -> assertBool "Read returned different object than Created" (doc == doc')
        it "can create and read out again (multiple times)" $ do
            let docs = [Document "entity" "source" (object ["num" .= n]) | n <- [1..10::Int]]
            fks' <- mapM (runDSMonad . createDocument source ) docs
            fks <- forM fks' $ \fk' -> case fk' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            allDifferent fks
            ress <- mapM (runDSMonad . readDocument source) fks
            forM_ (zip ress docs) $ \res -> case res of
                (Left err, _) -> assertFailure (show err)
                (Right doc', doc) -> assertBool "Read returned different object than Created" (doc == doc')

main :: IO ()
main = hspec suite
