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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import System.Process
import Test.Hspec

import Retcon.DataSource
import Retcon.Error
import Retcon.Monad
import Retcon.Options
import Retcon.Store
import Retcon.Store.PostgreSQL

dbname :: BS.ByteString
dbname = "retcon_test"

options :: RetconOptions
options = defaultOptions { optDB = "dbname=" `BS.append` dbname }

runAction :: PGStorage
          -> RetconMonad RWToken () r
          -> IO (Either RetconError r)
runAction store action = runRetconMonad options [] (token store) () action

-- | Canned query to run to check that connections are live.
onepluszero :: Connection -> IO [Only Int]
onepluszero conn = query_ conn "SELECT 1 + 0;"

-- | Check that a SqlError exception is about a disconnected connection.
isDisconnected :: SqlError -> Bool
isDisconnected e = sqlErrorMsg e == "connection disconnected"

-- | Count the objects in the store.
countStore :: PGStorage -> IO (Int, Int, Int, Int)
countStore (PGStore conn) = do
    [Only iks] <- query_ conn "SELECT count(*) FROM retcon;"
    [Only fks] <- query_ conn "SELECT count(*) FROM retcon_fk;"
    [Only docs] <- query_ conn "SELECT count(*) FROM retcon_initial;"
    [Only diffs] <- query_ conn "SELECT count(*) FROM retcon_diff;"
    return (iks, fks, docs, diffs)

main :: IO ()
main = hspec postgresqlSuite

prepareDatabase :: IO () -> IO ()
prepareDatabase action = bracket setupSuite teardownSuite (const action)
  where
    db = BS.unpack dbname

    setupSuite :: IO ()
    setupSuite = do
        _ <- system $ concat [ " dropdb --if-exists ", db, " >/dev/null 2>&1 "
                             , " && createdb ", db
                             , " && psql --quiet --file=retcon.sql ", db
                             ]
        return ()

    teardownSuite :: a -> IO ()
    teardownSuite _ = do
        _ <- system $ concat [ "dropdb --if-exists ", db, " >/dev/null 2>&1 " ]
        return ()

postgresqlSuite :: Spec
postgresqlSuite = around prepareDatabase $
    describe "PostgreSQL storage backend" $ do
        it "should be connected when initialised" $ do
            -- postgresql-simple doesn't seem to provide a function similar to
            -- PQstatus from libpq; so let's just try *using* the connection!
            store@(PGStore conn) <- storeInitialise options

            [Only one] <- onepluszero conn
            one `shouldBe` 1

            storeFinalise store

        it "should be disconnected when finalised" $ do
            store@(PGStore conn) <- storeInitialise options

            -- Check that the connection is now open.
            [Only one] <- onepluszero conn
            one `shouldBe` 1

            -- Check that finalising does close the connection.
            storeFinalise store
            --(void $ onepluszero conn) `shouldThrow` isDisconnected

        it "should allocate and delete internal keys" $ do
            store@(PGStore conn) <- storeInitialise options

            -- Check it's empty, so our counts will be correct.
            initial@(iks, fks, docs, diffs) <- countStore store
            initial `shouldBe` (0,0,0,0)

            -- Create some internal keys.
            keys <- runAction store $ do
                (ik1 :: InternalKey "tests") <- createInternalKey
                (ik2 :: InternalKey "tests") <- createInternalKey
                (ik3 :: InternalKey "testers") <- createInternalKey

                let (fk1 :: ForeignKey "tests" "test") = ForeignKey "fk1"
                let (fk2 :: ForeignKey "tests" "more") = ForeignKey "fk2"
                let (fk3 :: ForeignKey "testers" "tester1") = ForeignKey "fk3"

                recordForeignKey ik1 fk1
                recordForeignKey ik1 fk2
                recordForeignKey ik3 fk3

                return (ik1, ik2, ik3)

            try1 <- countStore store
            try1 `shouldBe` (3, 3, 0, 0)

            -- Delete ik1 and it's associated things.
            ik1 <- case keys of
                Left  _          -> error "Couldn't create internal keys"
                Right (k1, _, _) -> return k1

            result <- runAction store $ do
                deleteInternalKey ik1
            result `shouldBe` Right ()

            try2 <- countStore store
            try2 `shouldBe` (2, 1, 0, 0)

            storeFinalise store

        it "should associate foreign and internal keys" $ do
            store@(PGStore conn) <- storeInitialise options

            let (fk1 :: ForeignKey "tests" "test") = ForeignKey "test1"
            let (fk2 :: ForeignKey "tests" "test") = ForeignKey "test2"
            let (fk3 :: ForeignKey "tests" "test") = ForeignKey "test3"
            let (fk4 :: ForeignKey "tests" "more") = ForeignKey "more1"
            let (fk5 :: ForeignKey "tests" "more") = ForeignKey "more2"
            let (fk6 :: ForeignKey "tests" "more") = ForeignKey "more3"

            ik1 <- runAction store $ do
                (ik1 :: InternalKey "tests") <- createInternalKey
                (ik2 :: InternalKey "tests") <- createInternalKey
                (ik3 :: InternalKey "tests") <- createInternalKey

                recordForeignKey ik1 fk3
                recordForeignKey ik1 fk4
                recordForeignKey ik2 fk2
                recordForeignKey ik2 fk5
                recordForeignKey ik3 fk1
                recordForeignKey ik3 fk6

                return ik1

            -- Check there are as many things in the database as we expect.
            counts <- countStore store
            counts `shouldBe` (3, 6, 0, 0)

            -- TODO: check that the data is actually correct, not just the
            -- right size.

            result <- runAction store $ do
                deleteForeignKey fk5

            -- Check there are as many things in the database as we expect.
            result `shouldBe` Right ()
            counts <- countStore store
            counts `shouldBe` (3, 5, 0, 0)

            -- TODO: check that the data is actually correct, not just the
            -- right size.

            result <- runAction store $ do
                case ik1 of
                    Left  _ -> error "Was not able to create ik1."
                    Right k -> deleteInternalKey k

            -- Check there are as many things in the database as we expect.
            result `shouldBe` Right ()
            counts <- countStore store
            counts `shouldBe` (2, 3, 0, 0)

            storeFinalise store

        it "should record initial documents" $ do
            store@(PGStore conn) <- storeInitialise options
            storeFinalise store
            pendingWith "Unimplemented"

        it "should record diffs" $ do
            store@(PGStore conn) <- storeInitialise options
            storeFinalise store
            pendingWith "Unimplemented"

-- $ Entities and Data Sources
--
-- This test suite uses two data entities, each with two data sources, in all
-- tests. This helps check that we're distinguishing internal and foreign keys
-- correctly, etc.

instance RetconEntity "tests" where
    entitySources _ = []

instance RetconEntity "testers" where
    entitySources _ = []

instance RetconDataSource "tests" "test" where

    data DataSourceState "tests" "test" = TestsTest
    initialiseState = return TestsTest
    finaliseState _ = return ()

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()

instance RetconDataSource "tests" "more" where

    data DataSourceState "tests" "more" = TestsMore
    initialiseState = return TestsMore
    finaliseState _ = return ()

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()

instance RetconDataSource "testers" "tester1" where

    data DataSourceState "testers" "tester1" = TestersTester1
    initialiseState = return TestersTester1
    finaliseState _ = return ()

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()

instance RetconDataSource "testers" "tester2" where

    data DataSourceState "testers" "tester2" = TestersTester2
    initialiseState = return TestersTester2
    finaliseState _ = return ()

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()


