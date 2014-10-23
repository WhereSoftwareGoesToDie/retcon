--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Applicative
import Data.Binary
import Retcon.Diff
import Retcon.Document
import Retcon.Network.Server
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import TreeHelpers ()

instance Arbitrary RequestConflicted where
    arbitrary = return RequestConflicted

instance Arbitrary ResponseConflicted where
    arbitrary = ResponseConflicted <$> arbitrary

deriving instance Arbitrary ConflictedDiffOpID
deriving instance Arbitrary DiffID

instance Arbitrary Document where
    arbitrary = Document <$> arbitrary

instance Arbitrary (Diff ()) where
    arbitrary = sized $ \n -> do
        l <- arbitrary
        childs <- choose (0,n)
        ops <- sequence $ replicate childs $ arbitrary
        return $ Diff l ops

instance Arbitrary (DiffOp ()) where
    arbitrary = oneof [
                InsertOp <$> arbitrary <*> resize 10 arbitrary <*> arbitrary,
                DeleteOp <$> arbitrary <*> resize 10 arbitrary
                ]

instance Arbitrary RequestChange where
    arbitrary = RequestChange <$> arbitrary

instance Arbitrary ChangeNotification where
    arbitrary = ChangeNotification <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ResponseChange where
    arbitrary = return ResponseChange

instance Arbitrary RequestResolve where
    arbitrary = RequestResolve <$> arbitrary <*> arbitrary

instance Arbitrary ResponseResolve where
    arbitrary = return ResponseResolve

instance Arbitrary InvalidRequest where
    arbitrary = return InvalidRequest

instance Arbitrary InvalidResponse where
    arbitrary = return InvalidResponse

main :: IO ()
main = hspec suite

suite :: Spec
suite = do
    describe "Wire format identity tests" $ do
        prop "RequestConflicted" (wireId :: RequestConflicted -> Bool)
        prop "ResponseConflicted" (wireId :: ResponseConflicted -> Bool)
        prop "RequestChange" (wireId :: RequestChange -> Bool)
        prop "ResponseChange" (wireId :: ResponseChange -> Bool)
        prop "RequestResolve" (wireId :: RequestResolve -> Bool)
        prop "ResponseResolve" (wireId :: ResponseResolve -> Bool)
        prop "InvalidRequest" (wireId :: InvalidRequest -> Bool)
        prop "InvalidResponse" (wireId :: InvalidResponse -> Bool)

wireId :: (Eq w, Binary w) => w -> Bool
wireId op = (decode . encode $ op) == op
