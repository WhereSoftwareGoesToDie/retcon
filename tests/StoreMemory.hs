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
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import Data.IORef
import qualified Data.Map.Strict as M
import Test.Hspec

import Retcon.DataSource
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
            -- mutate
            finaliseStorage state
            pending

        it "should record diffs" $ do
            state <- (initialiseStorage options :: IO MemStorage)
            -- mutate
            finaliseStorage state
            pending

main :: IO ()
main = hspec memorySuite

pass :: Expectation
pass = return ()

