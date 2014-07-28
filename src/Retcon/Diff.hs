{-# LANGUAGE DeriveFunctor #-}
------------------------------------------------------------------------
-- |
-- Module      : Retcon.Diff
-- Description : Represent, build and apply diffs over documents.
-- Copyright   : Anchor Systems and others.
-- License     : BSD3
--
-- Maintainer  : Thomas Sutton <me@thomas-sutton.id.au>
-- Stability   : experimental
-- Portability : portable
--
-- This module implements the 'Diff' and 'DiffOp' data styles which
-- together model the changes between 'Document's. Both diffs and the
-- operations which compose them can be labelled with arbitary values.
------------------------------------------------------------------------
module Retcon.Diff where

import Data.Text (Text)

import Data.Tree.EdgeLabelled
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

-- | An empty 'Diff'.
emptyDiff :: Diff ()
emptyDiff = Diff () []

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
    let l   = label from
        from' = toList from
        to' = toList to
        ops = diffLists from' to'
    in Diff l $ fmap (fmap $ const l) ops

-- | Build a list of diff operations from two tree association lists.
diffLists :: [([DocumentKey],DocumentValue)]   -- ^ Source tree.
          -> [([DocumentKey],DocumentValue)]   -- ^ Target tree.
          -> [DiffOp ()]
diffLists from [] = map (\(n,_) -> DeleteOp () n) from
diffLists []   to = map (\(n,v) -> InsertOp () n v) to
diffLists ss@((ns,vs):sr) ds@((nd,vd):dr) = case compare ns nd of
    LT -> (DeleteOp () ns):(diffLists sr ds)
    GT -> (InsertOp () nd vd):(diffLists ss dr)
    EQ -> if (vs == vd)
          then diffLists sr dr
          else (InsertOp () nd vd):(diffLists sr dr)

-- | Apply a 'Diff' to a 'Document'.
applyDiff :: Diff l
          -> Document
          -> Document
applyDiff (Diff _ _ops) _doc = error "applyDiff: Not implemented"

