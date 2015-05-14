{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson.Diff (Operation (..))
import qualified Data.Aeson.Diff as D
import           Test.Hspec

import           Retcon.Diff
import           Retcon.Identifier

suite :: Spec
suite = do
    describe "ignoreConflicts merge policy" $ do
        let d1 = mkPatch Nothing
                 [ Ins [D.OKey "value"] "one"
                 , Ins [D.OKey "pet"] "cat"
                 , Del [D.OKey "type"] "zebra"
                 , Del [D.OKey "size"] "small"
                 , Ins [D.OKey "size"] "medium"
                 ]

            d2 = mkPatch Nothing
                 [ Ins [D.OKey "value"] "two"
                 , Del [D.OKey "food"] ""
                 , Ins [D.OKey "owner"] "Doge"
                 ]

            d3 = mkPatch Nothing
                 [ Ins [D.OKey "food"] "grass"
                 , Ins [D.OKey "type"] "goat"
                 , Del [D.OKey "color"] "pink"
                 , Del [D.OKey "value"] "$$$"
                 ]

            (accepted, rejected) = mergeWith ignoreConflicts [d1,d2,d3]

        it "should accept changes that do not conflict" $
            accepted `shouldBe` mkPatch Nothing
                                        [ Ins [D.OKey "pet"] "cat"
                                        , Del [D.OKey "size"] "small"
                                        , Ins [D.OKey "size"] "medium"
                                        , Ins [D.OKey "owner"] "Doge"
                                        , Del [D.OKey "color"] "pink"
                                        ]

        it "should ignore all changes which conflict" $
            rejected `shouldBe` map mkRej [ Ins [D.OKey "value"] "one"
                                          , Del [D.OKey "type"] "zebra"
                                          , Ins [D.OKey "value"] "two"
                                          , Del [D.OKey "food"] ""
                                          , Ins [D.OKey "food"] "grass"
                                          , Ins [D.OKey "type"] "goat"
                                          , Del [D.OKey "value"] "$$$"
                                          ]

    describe "rejectAll merge policy" $ do
        let d1    = mkPatch Nothing [Ins [D.OKey "value"] "one"]
            d2    = mkPatch Nothing [Ins [D.OKey "value"] "two"]
            (d,c) = merge rejectAll d1 d2

        it "should ignore all changes" $ do
            d `shouldBe` emptyPatch
            c `shouldBe` map mkRej [ Ins [D.OKey "value"] "one"
                                   , Ins [D.OKey "value"] "two" ]

    describe "acceptAll merge policy" $ do
        let d1    = mkPatch Nothing [Ins [D.OKey "value"] "one"]
            d2    = mkPatch Nothing [Ins [D.OKey "value"] "two"]
            (d,c) = merge acceptAll d1 d2

        it "should accept all changes" $ do
            d `shouldBe` mkPatch Nothing [ Ins [D.OKey "value"] "one"
                                 , Ins [D.OKey "value"] "two" ]
            c `shouldBe` []

    describe "trustOnly merge policy" $ do
        let d1    = Patch (Name "lord")
                  $ D.Patch [Ins [D.OKey "value"] "one"]
            d2    = Patch (Name "peasant")
                  $ D.Patch [Ins [D.OKey "value"] "two"]
            (d,c) = merge (trustOnlySource "lord") d1 d2

        it "should only accept changes from the specified source" $ do
            d `shouldBe` d1
            c `shouldBe` [ RejectedOp (Name "peasant") $ Ins [D.OKey "value"] "two" ]


mkPatch :: Maybe SourceName -> [Operation] -> Patch
mkPatch Nothing  = Patch Unamed   . D.Patch
mkPatch (Just n) = Patch (Name n) . D.Patch

mkRej :: Operation -> RejectedOp
mkRej = RejectedOp Unamed

main :: IO ()
main = hspec suite
