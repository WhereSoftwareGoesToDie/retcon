-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Data.Proxy
import System.IO.Error
import Test.Hspec
import Text.Trifecta

import Retcon.DataSource
import Retcon.Error
import Retcon.Options
import Utility.Configuration

testfile :: FilePath
testfile = "tests/data/parameters.conf"

explodeAfterLookup = do
    p <- ask
    let values = (M.lookup "password" p, M.lookup "init" p, M.lookup "final" p)
    error $ show values

instance RetconEntity "inittest" where
    entitySources _ = [SomeDataSource (Proxy :: Proxy "initsource")]

instance RetconDataSource "inittest" "initsource" where
    data DataSourceState "inittest" "initsource" = InitSrcState
    initialiseState = explodeAfterLookup
    finaliseState _ = return ()
    setDocument = error "Unimplemented"
    getDocument = error "Unimplemented"
    deleteDocument = error "Unimplemented"

initCfg :: [SomeEntity]
initCfg = [SomeEntity (Proxy :: Proxy "inittest")]

instance RetconEntity "finaltest" where
    entitySources _ = [SomeDataSource (Proxy :: Proxy "finalsource")]

instance RetconDataSource "finaltest" "finalsource" where
    data DataSourceState "finaltest" "finalsource" = FinalSrcState
    initialiseState = return FinalSrcState
    finaliseState _ = explodeAfterLookup
    setDocument = error "Unimplemented"
    getDocument = error "Unimplemented"
    deleteDocument = error "Unimplemented"

finalCfg :: [SomeEntity]
finalCfg = [SomeEntity (Proxy :: Proxy "finaltest")]

configurationSuite :: Spec
configurationSuite = do
    describe "Configuration loading" $ do
        it "should parse data" $ do
            params <- parseFromFile configParser testfile
            params `shouldBe` Just
                [ ("inittest", "initsource", "uri", "https://api.com/")
                , ("inittest", "initsource", "password", "p4ssw0rd")
                , ("inittest", "initsource", "init", "initial object")
                , ("finaltest", "finalsource", "server", "db1.example.com")
                , ("finaltest", "finalsource", "password", "databass")
                , ("finaltest", "finalsource", "final", "final object")
                ]

        it "should transform parsed data" $ do
            params <- fmap convertConfig <$> parseFromFile configParser testfile
            params `shouldBe` (Just $ M.fromList
                [ (("inittest", "initsource"), M.fromList
                    [ ("uri", "https://api.com/")
                    , ("password", "p4ssw0rd")
                    , ("init", "initial object")
                    ])
                , (("finaltest", "finalsource"), M.fromList
                    [ ("server", "db1.example.com")
                    , ("password", "databass")
                    , ("final", "final object")
                    ])
                ])

    describe "Configuration passing" $ do
        it "should pass data when initialising a data source" $ do
            Just params <- fmap convertConfig <$> parseFromFile configParser testfile
            -- Call initialise for testing entity; check that exceptions occur
            -- with known values.
            let values = ( Just "p4ssw0rd" :: Maybe String
                         , Just "initial object" :: Maybe String
                         , Nothing :: Maybe String
                         )
            initTest params initCfg `shouldThrow` valueError (show values)

        it "should pass data when finalising a data source" $ do
            Just params <- fmap convertConfig <$> parseFromFile configParser testfile
            -- Call finalise for testing entity; check that exceptions occur
            -- with known values.
            let values = ( Just "databass" :: Maybe String
                         , Nothing :: Maybe String
                         , Just "final object" :: Maybe String
                         )
            initTest params finalCfg `shouldThrow` valueError (show values)
  where
    valueError msg = (== ErrorCall msg)

initTest :: ParamMap
         -> [SomeEntity]
         -> IO ()
initTest params cfg = do
    state <- initialiseEntities params cfg
    void $ finaliseEntities params state

main :: IO ()
main = hspec configurationSuite
