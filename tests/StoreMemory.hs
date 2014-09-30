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
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Test.Hspec

import Retcon.DataSource
import Retcon.Document
import Retcon.Error
import Retcon.Monad
import Retcon.Options
import Retcon.Store
import Retcon.Store.Memory

options :: RetconOptions
options = defaultOptions


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

-- $ Test Helpers

runAction :: MemStorage
          -> RetconMonad RWToken () r
          -> IO (Either RetconError r)
runAction store action = runRetconMonad options [] (token store) () action

-- $ Memory Storage Tests

memorySuite :: Spec
memorySuite =
    describe "Memory storage backend" $ do
        it "should be empty when initialised" $ do
            state <- storeInitialise options :: IO MemStorage

            store <- readIORef . unwrapMemStorage $ state
            store `shouldBe` emptyState

        it "should be empty when finalised" $ do
            state <- storeInitialise options :: IO MemStorage
            let ref = unwrapMemStorage state

            -- Should be empty before we've done anything to it.
            store <- readIORef ref
            store `shouldBe` emptyState

            runAction state $ do
                (_ :: InternalKey "tests") <- createInternalKey
                (_ :: InternalKey "tests") <- createInternalKey
                (_ :: InternalKey "testers") <- createInternalKey
                return ()

            -- Should not be empty after we've put things in it.
            store <- readIORef ref
            store `shouldSatisfy` (/= emptyState)
            memNextKey store `shouldBe` 3

            storeFinalise state

            -- Should be empty after we've finalised it.
            store <- readIORef ref
            store `shouldBe` emptyState

        it "should allocate and delete internal keys" $ do
            state <- storeInitialise options :: IO MemStorage
            let ref = unwrapMemStorage state

            runAction state $ do
                (ik1 :: InternalKey "tests") <- createInternalKey
                (ik2 :: InternalKey "testers") <- createInternalKey
                (ik3 :: InternalKey "tests") <- createInternalKey

                let (fk1 :: ForeignKey "tests" "test") = ForeignKey "fk1"
                let (fk2 :: ForeignKey "testers" "tester1") = ForeignKey "fk2"
                let (fk3 :: ForeignKey "tests" "test") = ForeignKey "fk3"

                recordForeignKey ik1 fk3
                recordForeignKey ik2 fk2
                recordForeignKey ik3 fk1

            store <- readIORef ref
            memNextKey store `shouldBe` 3
            memItoF store `shouldBe` M.fromList [
                (0, [("tests", "test", "fk3")]),
                (1, [("testers", "tester1", "fk2")]),
                (2, [("tests", "test", "fk1")])
                ]
            memFtoI store `shouldBe` M.fromList [
                (("tests", "test", "fk1"), 2),
                (("testers", "tester1", "fk2"), 1),
                (("tests", "test", "fk3"), 0)
                ]

        it "should associate foreign and internal keys" $ do
            state <- storeInitialise options :: IO MemStorage
            let ref = unwrapMemStorage state

            let (fk1 :: ForeignKey "tests" "test") = ForeignKey "test1"
            let (fk2 :: ForeignKey "tests" "test") = ForeignKey "test2"
            let (fk3 :: ForeignKey "tests" "test") = ForeignKey "test3"
            let (fk4 :: ForeignKey "tests" "more") = ForeignKey "more1"
            let (fk5 :: ForeignKey "tests" "more") = ForeignKey "more2"
            let (fk6 :: ForeignKey "tests" "more") = ForeignKey "more3"

            ik1 <- runAction state $ do
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

            -- Check all values have been inserted.
            store <- readIORef ref
            memNextKey store `shouldBe` 3
            memFtoI store `shouldBe` M.fromList
                [ (("tests", "test", "test1"), 2)
                , (("tests", "test", "test2"), 1)
                , (("tests", "test", "test3"), 0)
                , (("tests", "more", "more1"), 0)
                , (("tests", "more", "more2"), 1)
                , (("tests", "more", "more3"), 2)
                ]
            memItoF store `shouldBe` M.fromList [
                (0, [ ("tests", "more", "more1")
                    , ("tests", "test", "test3")
                    ]),
                (1, [ ("tests", "more", "more2")
                    , ("tests", "test", "test2")
                    ]),
                (2, [ ("tests", "more", "more3")
                    , ("tests", "test", "test1")
                    ])
                ]

            -- Delete some of them.
            runAction state $ do
                deleteForeignKey fk5

            -- Check that we have the right things.
            store <- readIORef ref
            memNextKey store `shouldBe` 3
            memFtoI store `shouldBe` M.fromList
                [ (("tests", "test", "test1"), 2)
                , (("tests", "test", "test2"), 1)
                , (("tests", "test", "test3"), 0)
                , (("tests", "more", "more1"), 0)
                , (("tests", "more", "more3"), 2)
                ]
            memItoF store `shouldBe` M.fromList [
                (0, [ ("tests", "more", "more1")
                    , ("tests", "test", "test3")
                    ]),
                (1, [ ("tests", "test", "test2")
                    ]),
                (2, [ ("tests", "more", "more3")
                    , ("tests", "test", "test1")
                    ])
                ]

            -- Delete an internal key.
            runAction state $ do
                case ik1 of
                    Left _ -> error "Didn't create key, so now we can't delete"
                    Right k -> deleteForeignKeys k

            -- Check that the bits got deleted.
            store <- readIORef ref
            memNextKey store `shouldBe` 3
            memFtoI store `shouldBe` M.fromList
                [ (("tests", "test", "test1"), 2)
                , (("tests", "test", "test2"), 1)
                , (("tests", "more", "more3"), 2)
                ]
            memItoF store `shouldBe` M.fromList [
                (1, [ ("tests", "test", "test2")
                    ]),
                (2, [ ("tests", "more", "more3")
                    , ("tests", "test", "test1")
                    ])
                ]

        it "should record initial documents" $ do
            state <- storeInitialise options :: IO MemStorage
            let ref = unwrapMemStorage state

            -- Should be empty before we've done anything to it.
            store <- readIORef ref
            store `shouldBe` emptyState

            -- Insert some initial documents.
            Right (ik1, ik2, ik3, ik4) <- runAction state $ do
                (ik1 :: InternalKey "tests") <- createInternalKey
                (ik2 :: InternalKey "testers") <- createInternalKey
                (ik3 :: InternalKey "tests") <- createInternalKey
                (ik4 :: InternalKey "tests") <- createInternalKey
                return (ik1, ik2, ik3, ik4)

            let doc1 = fromJust $ mkNode (Just "Document One")
            let doc2 = fromJust $ mkNode (Just "Document Two")
            let doc3 = fromJust $ mkNode (Just "Document Three")
            let doc4 = fromJust $ mkNode (Just "Document Four")

            --
            -- Check documents can be inserted.
            --

            runAction state $ do
                recordInitialDocument ik1 doc1
                recordInitialDocument ik2 doc2
                recordInitialDocument ik3 doc3

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memInits store `shouldBe` M.fromList
                [ (("tests", 0), doc1)
                , (("testers", 1), doc2)
                , (("tests", 2), doc3)
                ]

            --
            -- Check documents can be updated.
            --
 
            runAction state $ do
                recordInitialDocument ik3 doc4

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memInits store `shouldBe` M.fromList
                [ (("tests", 0), doc1)
                , (("testers", 1), doc2)
                , (("tests", 2), doc4)
                ]

            --
            -- Check documents can be deleted.
            --

            runAction state $ do
                deleteInitialDocument ik2

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memInits store `shouldBe` M.fromList
                [ (("tests", 0), doc1)
                , (("tests", 2), doc4)
                ]

            storeFinalise state

        it "should record diffs" $ do
            state <- storeInitialise options :: IO MemStorage
            let ref = unwrapMemStorage state

            -- Should be empty before we've done anything to it.
            store <- readIORef ref
            store `shouldBe` emptyState

            -- Insert some initial documents.
            Right (ik1, ik2, ik3, ik4) <- runAction state $ do
                (ik1 :: InternalKey "tests") <- createInternalKey
                (ik2 :: InternalKey "testers") <- createInternalKey
                (ik3 :: InternalKey "tests") <- createInternalKey
                (ik4 :: InternalKey "tests") <- createInternalKey
                return (ik1, ik2, ik3, ik4)

            let a1 = mempty
            let l1 = []
            let ds1 = (a1, l1)
            let a2 = mempty
            let l2 = [mempty]
            let ds2 = (a2, l2)
            let a3 = mempty
            let l3 = [mempty]
            let ds3 = (a3, l3)

            --
            -- Check diffs can be recorded.
            --

            runAction state $ do
                recordDiffs ik1 ds1
                recordDiffs ik2 ds2
                recordDiffs ik3 ds3

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memDiffs store `shouldBe` M.fromList
                [ (("tests", 0), [ds1])
                , (("testers", 1), [ds2])
                , (("tests", 2), [ds3])
                ]

            --
            -- Check diffs can be deleted.
            --

            _ <- runAction state $ do
                deleteDiffs ik2

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memDiffs store `shouldBe` M.fromList
                [ (("tests", 0), [ds1])
                , (("tests", 2), [ds3])
                ]

            storeFinalise state

main :: IO ()
main = hspec memorySuite
