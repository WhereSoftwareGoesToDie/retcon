{-# LANGUAGE OverloadedStrings #-}

-- | Description: Program to generate a diff between two JSON documents.
module Main where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Vector as V
import System.Environment

type Path = [Step]

data Step
    = AIndex Int
    | OKey Text
  deriving (Eq, Show)

-- | An atomic change to a JSON document.
data Change
    = Ins { changePath :: Path, changeValue :: Value } -- ^ Insert a value at a location.
    | Del { changePath :: Path, oldValue :: Value } -- ^ Delete the value at a location.
    | Change { changePath :: Path, oldValue :: Value, changeValue :: Value }
  deriving (Eq, Show)

-- | A 'Diff' is a list of changes.
data Diff = Diff { diffChanges :: [Change] }
  deriving (Eq)

instance Monoid Diff where
    mempty = Diff []
    mappend (Diff c1) (Diff c2) = Diff $ mappend c1 c2

instance Show Diff where
    show (Diff []) = ""
    show (Diff ls) = unlines $ map show ls

ins p v = Diff [Ins p v]

del p v = Diff [Del p v]

ch p v1 v2 = Diff [Del p v1, Ins p v2]

-- | Diff two JSON documents.
diffJson
    :: Value
    -> Value
    -> Diff
diffJson = worker []
  where
    check :: Monoid m => Bool -> m -> m
    check b v = if b then mempty else v

    worker :: Path -> Value -> Value -> Diff
    worker p v1 v2 = case (v1, v2) of
        -- For atomic values of the same type, emit changes if they differ.
        (Null,      Null)      -> mempty
        (Bool b1,   Bool b2)   -> check (b1 == b2) $ ch p v1 v2
        (Number n1, Number n2) -> check (n1 == n2) $ ch p v1 v2
        (String s1, String s2) -> check (s1 == s2) $ ch p v1 v2

        -- For structured values of the same type, walk them.
        (Array a1,  Array a2)  -> check (a1 == a2) $ workArray p a1 a2
        (Object o1, Object o2) -> check (o1 == o2) $ workObject p o1 o2

        -- For values of different types, delete v1 and insert v2.
        _                      -> del p v1 <> ins p v2

    -- Walk the keys in two objects, producing a 'Diff'.
    workObject :: Path -> Object -> Object -> Diff
    workObject p o1 o2 =
        let k1 = HM.keys o1
            k2 = HM.keys o2
            dk = filter (not . (`elem` k2)) k1
            ik = filter (not . (`elem` k1)) k2
            ck = filter (`elem` k2) k1
            ds = map (\k -> Del (p ++ [OKey k]) . fromJust $ HM.lookup k o1) dk
            is = map (\k -> Ins (p ++ [OKey k]) . fromJust $ HM.lookup k o2) ik
            cs = map (\k -> worker (p ++ [OKey k]) (fromJust $ HM.lookup k o1) (fromJust $ HM.lookup k o2)) ck
        in Diff $ ds ++ is ++ (diffChanges . mconcat $ cs)

    -- Walk the indexes in two arrays, producing a 'Diff'.
    workArray :: Path -> Array -> Array -> Diff
    workArray p _ _ = mempty

main :: IO ()
main = do
    [f1,f2] <- getArgs
    d1 <- BSL.fromStrict <$> BS.readFile f1
    d2 <- BSL.fromStrict <$> BS.readFile f2

    let Just v1 = decode d1
    let Just v2 = decode d2

    let diff = diffJson v1 v2
    print diff

