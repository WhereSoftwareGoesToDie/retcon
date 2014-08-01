--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- |
-- Description : Represent, build and apply diffs over documents.
--
-- This module implements the 'Diff' and 'DiffOp' data styles which
-- together model the changes between 'Document's. Both diffs and the
-- operations which compose them can be labelled with arbitary values.

{-# LANGUAGE DeriveFunctor #-}

module Retcon.Diff where

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import GHC.Exts (IsList (..))

import Data.Tree.GenericTrie
import Retcon.Document

-- | A 'Diff' describes a collection of changes to a 'Document'.
data Diff l = Diff
  { diffLabel   :: l
  , diffChanges :: [DiffOp l]
  }
  deriving (Eq, Show, Functor)

-- | A 'DiffOp' describes a single change to be applied to a 'Document'.
data DiffOp l
  = InsertOp l [DocumentKey] Text -- ^ Set a field to a value.
  | DeleteOp l [DocumentKey]      -- ^ Unset a field.
  deriving (Eq, Show, Functor)

instance Monoid l => Monoid (Diff l) where
    mempty = Diff mempty mempty
    mappend = error "unimplemented mappend for Diff"

-- | Generate a 'Diff' from two documents, with a void label.
diff :: Document -- ^ Source document.
     -> Document -- ^ Target document.
     -> Diff ()
diff = diffWith (const ())

-- | Generate a 'Diff' from two documents, using the supplied function
-- to extract a label.
diffWith :: (Document -> l) -- ^ Extract a label from target document
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
diffLists :: [([DocumentKey],DocumentValue)]   -- ^ Source tree.
          -> [([DocumentKey],DocumentValue)]   -- ^ Target tree.
          -> [DiffOp ()]
diffLists from [] = map (\(n,_) -> DeleteOp () n) from
diffLists []   to = map (\(n,v) -> InsertOp () n v) to
diffLists ss@((ns,vs):sr) ds@((nd,vd):dr) =
    case compare ns nd of
        LT -> DeleteOp () ns:diffLists sr ds
        GT -> InsertOp () nd vd:diffLists ss dr
        EQ -> if vs == vd
            then diffLists sr dr
            else InsertOp () nd vd:diffLists sr dr

-- | Apply a 'Diff' to a 'Document'.
applyDiff :: Diff l
          -> Document
          -> Document
applyDiff (Diff _ []) doc  = doc
applyDiff (Diff _ ops) doc = foldl (flip evalDiffOp) doc ops

-- | Apply a single 'DiffOp' to a 'Document'.
evalDiffOp :: DiffOp l
           -> Document
           -> Document

-- Delete the value at the current location.
evalDiffOp (DeleteOp _ []) (Document (Node _ kids))
  = Document . Node Nothing $ M.filter (not . emptyNode) kids

-- Navigate to a location (so you can delete it's value).
evalDiffOp (DeleteOp _ (k:ks)) (Document (Node l kids))
  = Document . Node l $ M.filter (not . emptyNode) $ M.alter (Just . updateChild) k kids
  where
    updateChild = unDocument . evalDiffOp (DeleteOp () ks) . Document . fromMaybe mempty

-- Set the value at the current location.
evalDiffOp (InsertOp _ []  v) (Document (Node _ kids)) =
    Document $ Node (Just v) kids

-- Navigate to a location (so you can set it's value).
evalDiffOp (InsertOp _ (k:ks) v) (Document (Node l kids))
  = Document . Node l $ M.alter (Just . updateChild) k kids
  where
    updateChild = unDocument . evalDiffOp (InsertOp () ks v) . Document . fromMaybe mempty

