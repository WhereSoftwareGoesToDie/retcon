--
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
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import Test.Hspec

import Retcon.Config
import Retcon.DataSource
import Retcon.Document
import Retcon.Error
import Retcon.Handler
import Retcon.Monad
import Retcon.Options
import TestHelpers

import Control.Applicative
import Control.Exception
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Proxy
import Data.String
import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.TypeLits
import System.IO
import System.Process

instance RetconEntity "alicorn_invoice" where
    entitySources _ = [ SomeDataSource (Proxy :: Proxy "alicorn_source") ]

instance RetconDataSource "alicorn_invoice" "alicorn_source" where
    getDocument key = error "We're not calling this"
    setDocument doc key = error "We're not calling this"
    deleteDocument key = error "We're not calling this"

suite :: Spec
suite = do
    describe "Retcon handling" $ do
        it "reads and writes initial documents" $ do
            let testDoc = Document [(["key"], "value")]
            result <- testHandler $ do
                conn <- asks snd

                result <- liftIO $ query conn "INSERT INTO retcon (entity) VALUES (?) RETURNING id" (Only "alicorn_invoice" :: Only String)
                case result of
                    [Only ik_base] -> do
                        let ik = InternalKey ik_base :: InternalKey "alicorn_invoice"

                        maybePut <- putInitialDocument ik testDoc
                        maybeGet <- getInitialDocument ik
                        return $ (maybePut, maybeGet)
                    _ -> error "I have no idea what happened here"
            case result of
                Left e -> error (show e)
                Right (maybe_put, maybe_get) -> do
                    maybe_put `shouldBe` ()
                    maybe_get `shouldBe` (Just testDoc)

        it "deletes initial documents" $ do
            let testDoc = Document [(["isThisGoingToGetDeleted"], "yes")]
            result <- testHandler $ do
                conn <- asks snd

                result <- liftIO $ query conn "INSERT INTO retcon (entity) VALUES (?) RETURNING id" (Only "alicorn_invoice" :: Only String)
                case result of
                    [Only ik_base] -> do
                        let ik = InternalKey ik_base :: InternalKey "alicorn_invoice"

                        maybePut <- putInitialDocument ik testDoc
                        maybeDel <- deleteInitialDocument ik
                        maybeGet <- getInitialDocument ik
                        return $ (maybePut, maybeDel, maybeGet)
                    _ -> error "I have no idea what happened here"
            case result of
                Left e -> error (show e)
                Right (maybe_put, maybe_del, maybe_get) -> do
                    maybe_put `shouldBe` ()
                    maybe_del `shouldBe` ()
                    maybe_get `shouldBe` Nothing

testHandler :: RetconHandler a -> IO (Either RetconError a)
testHandler a = bracket setupConn closeConn run
    where
        setupConn = connectPostgreSQL testConnection
        closeConn = close
        run conn = do
            dbschema <- fromString <$> readFile "retcon.sql"
            _ <- execute_ conn dbschema
            let cfg = RetconConfig []
            result <- runRetconHandler defaultOptions cfg conn a
            return result

testDBName :: ByteString
testDBName = "retcon_handler_test"

testConnection :: ByteString
testConnection = BS.concat [BS.pack "dbname='", testDBName, BS.pack "'"]

setupSuite :: IO ()
setupSuite = do
    _ <- system $ concat ["dropdb --if-exists ", BS.unpack testDBName, " && createdb ", BS.unpack testDBName]
    return ()

teardownSuite :: () -> IO ()
teardownSuite () = do
    _ <- system $ concat ["dropdb ", BS.unpack testDBName]
    return ()

prepMe :: IO () -> IO ()
prepMe action = bracket setupSuite teardownSuite (const action)

main :: IO ()
main = hspec $ around prepMe $ do
    suite
