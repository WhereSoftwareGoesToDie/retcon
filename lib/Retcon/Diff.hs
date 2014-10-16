--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- |
-- Description: Represent, build and apply diffs over documents.
--
-- This module implements the 'Diff' and 'DiffOp' data types which together
-- model changes between 'Document's. Both diffs and the operations which
-- compose them can be labelled with arbitary values.

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Retcon.Diff (
    Diff (..),
    DiffOp (..),
    FromJSON,
    ToJSON,
    Monoid,
    diffOpIsInsert,
    diffOpIsDelete,
    diffOpTarget,
    diffOpAffects,
    diff,
    applyDiff
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Tree.GenericTrie
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import           GHC.Exts (IsList (..))
import           Retcon.Document

-- | A 'Diff' describes a collection of changes to a 'Document'.
data Diff l = Diff
  { diffLabel   :: l
  , diffChanges :: [DiffOp l]
  }
  deriving (Eq, Show, Functor)

instance FromJSON l => FromJSON (Diff l) where
    parseJSON (Object v) = Diff <$> v .: "diff_label" <*> v .: "changes"
    parseJSON _          = mzero

instance ToJSON l => ToJSON (Diff l) where
    toJSON (Diff l changes) = object [ "diff_label" .= toJSON l
                                     , "changes"    .= changes
                                     ]

-- | A 'DiffOp' describes a single change to be applied to a 'Document'.
data DiffOp l
  = InsertOp l [DocumentKey] Text -- ^ Set a field to a value.
  | DeleteOp l [DocumentKey]      -- ^ Unset a field.
  deriving (Eq, Show, Functor)

instance Monoid l => Monoid (Diff l) where
    mempty = Diff mempty mempty
    (Diff l1 o1) `mappend` (Diff l2 o2) = Diff (l1 `mappend` l2) (o1 ++ o2)

instance FromJSON l => FromJSON (DiffOp l) where
    parseJSON (Object v) = case HM.lookup "op" v of
        Just "Insert" -> InsertOp <$> v .: "op_label" <*> v .: "keys" <*> v .: "text"
        Just "Delete" -> DeleteOp <$> v .: "op_label" <*> v .: "keys"
        _ -> mzero
    parseJSON _          = mzero

instance ToJSON l => ToJSON (DiffOp l) where
    toJSON (InsertOp l keys text) = object [ "op"       .= String "Insert"
                                           , "op_label" .= toJSON l
                                           , "keys"     .= keys
                                           , "text"     .= text
                                           ]
    toJSON (DeleteOp l keys)      = object [ "op"       .= String "Delete"
                                           , "op_label" .= toJSON l
                                           , "keys"     .= keys
                                           ]

-- | Predicate: Operation is an insertion.
diffOpIsInsert
    :: DiffOp l
    -> Bool
diffOpIsInsert op = case op of
    DeleteOp {} -> False
    InsertOp {} -> True

-- | Predicate: Operation is a deletion.
diffOpIsDelete
    :: DiffOp l
    -> Bool
diffOpIsDelete op = case op of
    DeleteOp {} -> True
    InsertOp {} -> False

-- | Extract the key which will be modified by a 'DiffOp'.
diffOpTarget
    :: DiffOp l
    -> [DocumentKey]
diffOpTarget (InsertOp _ k _) = k
diffOpTarget (DeleteOp _ k) = k

-- | Predicate: does the operation touch one of these keys.
diffOpAffects
    :: DiffOp l
    -> [[DocumentKey]]
    -> Bool
diffOpAffects op keys = diffOpTarget op `elem` keys

-- | Generate a 'Diff' from two documents, with a void label.
diff
    :: Document -- ^ Source document.
    -> Document -- ^ Target document.
    -> Diff ()
diff = diffWith (const ())

-- | Generate a 'Diff' from two documents, using the supplied function
-- to extract a label.
diffWith
    :: (Document -> l) -- ^ Extract a label from target document
    -> Document        -- ^ Source document.
    -> Document        -- ^ Target document.
    -> Diff l
diffWith label from to =
    let l     = label from
        from' = toList . unDocument $ from
        to'   = toList . unDocument $ to
        ops   = diffLists from' to'
    in Diff l $ fmap (fmap $ const l) ops

-- | Build a list of diff operations from two tree association lists.
diffLists
    :: [([DocumentKey],DocumentValue)]   -- ^ Source tree.
    -> [([DocumentKey],DocumentValue)]   -- ^ Target tree.
    -> [DiffOp ()]
diffLists from [] = map (\(n,_) -> DeleteOp () n) from
diffLists []   to = map (uncurry $ InsertOp ()) to
diffLists ss@((ns,vs):sr) ds@((nd,vd):dr) =
    case compare ns nd of
        LT -> DeleteOp () ns:diffLists sr ds
        GT -> InsertOp () nd vd:diffLists ss dr
        EQ -> if vs == vd
            then diffLists sr dr
            else InsertOp () nd vd:diffLists sr dr

-- | Apply a 'Diff' to a 'Document'.
applyDiff
    :: Diff l
    -> Document
    -> Document
applyDiff (Diff _ []) doc  = doc
applyDiff (Diff _ ops) doc = foldl (flip evalDiffOp) doc ops

-- | Apply a single 'DiffOp' to a 'Document'.
-- Handles four different cases:
-- * Insert at current node
-- * Insert within one of the current node's children
-- * Delete from current node
-- * Delete within one of the current node's children
evalDiffOp
    :: DiffOp l
    -> Document
    -> Document

-- Set the value at the current location.
evalDiffOp (InsertOp _ []  v) (Document (Node _ kids)) =
    Document $ Node (Just v) kids

-- Navigate to a location (so you can set its value).
evalDiffOp (InsertOp _ (k:ks) v) (Document (Node l kids))
  = Document . Node l $ M.alter (updateChild (InsertOp () ks v)) k kids

-- Delete the value at the current location.
evalDiffOp (DeleteOp _ []) (Document (Node _ kids))
  = Document . Node Nothing $ M.filter (not . emptyNode) kids

-- Navigate to a location (so you can delete its value).
evalDiffOp (DeleteOp _ (k:ks)) (Document (Node l kids)) =
    Document
    . Node l
    . M.filter (not . emptyNode) $ M.alter (updateChild (DeleteOp () ks)) k kids

updateChild
    :: DiffOp a
    -> Maybe (Tree DocumentKey DocumentValue)
    -> Maybe (Tree DocumentKey DocumentValue)
updateChild op =
    Just . unDocument . evalDiffOp op . Document . fromMaybe mempty

