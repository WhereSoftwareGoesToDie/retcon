--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import Data.IORef
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Monoid
import Test.Hspec

import Retcon.DataSource
import Retcon.Document
import Retcon.Options
import Retcon.Store
import Retcon.Store.Memory

options :: RetconOptions
options = defaultOptions

instance RetconEntity "tests" where
    entitySources _ = []

instance RetconDataSource "tests" "test" where

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()

instance RetconDataSource "tests" "more" where

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()

memorySuite :: Spec
memorySuite = do
    describe "Memory storage backend" $ do
        it "should be empty when initialised" $ do
            state <- (initialiseStorage options :: IO MemStorage)

            store <- readIORef . unwrapMemStorage $ state
            store `shouldBe` emptyState

        it "should be empty when finalised" $ do
            state <- (initialiseStorage options :: IO MemStorage)
            let ref = unwrapMemStorage $ state

            -- Should be empty before we've done anything to it.
            store <- readIORef ref
            store `shouldBe` emptyState

            (_ :: InternalKey "tests") <- createInternalKey state
            (_ :: InternalKey "tests") <- createInternalKey state

            -- Should not be empty after we've put things in it.
            store <- readIORef ref
            store `shouldSatisfy` (/= emptyState)

            finaliseStorage state

            -- Should be empty after we've finalised it.
            store <- readIORef ref
            store `shouldBe` emptyState

        it "should allocate and delete internal keys" $ do
            state <- (initialiseStorage options :: IO MemStorage)
            let ref = unwrapMemStorage $ state

            (ik1 :: InternalKey "tests") <- createInternalKey state
            (ik2 :: InternalKey "tests") <- createInternalKey state
            (ik3 :: InternalKey "tests") <- createInternalKey state

            let (fk1 :: ForeignKey "tests" "test") = ForeignKey "fk1"
            let (fk2 :: ForeignKey "tests" "test") = ForeignKey "fk2"
            let (fk3 :: ForeignKey "tests" "test") = ForeignKey "fk3"

            recordForeignKey state ik1 fk3
            recordForeignKey state ik2 fk2
            recordForeignKey state ik3 fk1

            store <- readIORef ref
            memNextKey store `shouldBe` 3
            memItoF store `shouldBe` M.fromList [
                (0, [("tests", "test", "fk3")]),
                (1, [("tests", "test", "fk2")]),
                (2, [("tests", "test", "fk1")])
                ]
            memFtoI store `shouldBe` M.fromList [
                (("tests", "test", "fk1"), 2),
                (("tests", "test", "fk2"), 1),
                (("tests", "test", "fk3"), 0)
                ]

        it "should associate foreign and internal keys" $ do
            state <- (initialiseStorage options :: IO MemStorage)
            let ref = unwrapMemStorage $ state

            -- mutate
            (ik1 :: InternalKey "tests") <- createInternalKey state
            (ik2 :: InternalKey "tests") <- createInternalKey state
            (ik3 :: InternalKey "tests") <- createInternalKey state

            let (fk1 :: ForeignKey "tests" "test") = ForeignKey "test1"
            let (fk2 :: ForeignKey "tests" "test") = ForeignKey "test2"
            let (fk3 :: ForeignKey "tests" "test") = ForeignKey "test3"
            let (fk4 :: ForeignKey "tests" "more") = ForeignKey "more1"
            let (fk5 :: ForeignKey "tests" "more") = ForeignKey "more2"
            let (fk6 :: ForeignKey "tests" "more") = ForeignKey "more3"

            recordForeignKey state ik1 fk3
            recordForeignKey state ik1 fk4
            recordForeignKey state ik2 fk2
            recordForeignKey state ik2 fk5
            recordForeignKey state ik3 fk1
            recordForeignKey state ik3 fk6

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
            deleteForeignKey state fk5

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
            deleteForeignKeys state ik1

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
            state <- (initialiseStorage options :: IO MemStorage)
            let ref = unwrapMemStorage $ state

            -- Should be empty before we've done anything to it.
            store <- readIORef ref
            store `shouldBe` emptyState

            -- Insert some initial documents.
            (ik1 :: InternalKey "tests") <- createInternalKey state
            (ik2 :: InternalKey "tests") <- createInternalKey state
            (ik3 :: InternalKey "tests") <- createInternalKey state
            (ik4 :: InternalKey "tests") <- createInternalKey state

            let doc1 = fromJust $ mkNode (Just "Document One")
            let doc2 = fromJust $ mkNode (Just "Document Two")
            let doc3 = fromJust $ mkNode (Just "Document Three")
            let doc4 = fromJust $ mkNode (Just "Document Four")

            --
            -- Check documents can be inserted.
            --

            recordInitialDocument state ik1 doc1
            recordInitialDocument state ik2 doc2
            recordInitialDocument state ik3 doc3

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memInits store `shouldBe` M.fromList
                [ (("tests", 0), doc1)
                , (("tests", 1), doc2)
                , (("tests", 2), doc3)
                ]

            --
            -- Check documents can be updated.
            --

            recordInitialDocument state ik3 doc4

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memInits store `shouldBe` M.fromList
                [ (("tests", 0), doc1)
                , (("tests", 1), doc2)
                , (("tests", 2), doc4)
                ]

            --
            -- Check documents can be deleted.
            --

            deleteInitialDocument state ik2

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memInits store `shouldBe` M.fromList
                [ (("tests", 0), doc1)
                , (("tests", 2), doc4)
                ]

            finaliseStorage state

        it "should record diffs" $ do
            state <- (initialiseStorage options :: IO MemStorage)
            let ref = unwrapMemStorage $ state

            -- Should be empty before we've done anything to it.
            store <- readIORef ref
            store `shouldBe` emptyState

            -- Insert some initial documents.
            (ik1 :: InternalKey "tests") <- createInternalKey state
            (ik2 :: InternalKey "tests") <- createInternalKey state
            (ik3 :: InternalKey "tests") <- createInternalKey state
            (ik4 :: InternalKey "tests") <- createInternalKey state

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

            recordDiffs state ik1 ds1
            recordDiffs state ik2 ds2
            recordDiffs state ik3 ds3

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memDiffs store `shouldBe` M.fromList
                [ (("tests", 0), [ds1])
                , (("tests", 1), [ds2])
                , (("tests", 2), [ds3])
                ]

            --
            -- Check diffs can be deleted.
            --

            deleteDiffs state ik2

            store <- readIORef ref
            memNextKey store `shouldBe` 4
            memDiffs store `shouldBe` M.fromList
                [ (("tests", 0), [ds1])
                , (("tests", 2), [ds3])
                ]

            finaliseStorage state

main :: IO ()
main = hspec memorySuite
