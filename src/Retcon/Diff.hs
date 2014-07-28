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

import           Data.List
import qualified Data.Map  as M
import           Data.Text (Text)

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
  let l = label from
      from' = toKVList from
      to' = toKVList to
      ops = diffKVList from' to'
  in Diff l $ fmap (fmap $ const l) ops

-- | Apply a 'Diff' to a 'Document'.
applyDiff :: Diff l
          -> Document
          -> Document
applyDiff (Diff _ []) doc  = doc
applyDiff _patch      _doc = error "Unable to apply diffs."

-- | Convert a 'Document' into a list of key/value pairs, sorted by key.
toKVList :: Document
         -> [([Text], Text)]
toKVList (Document vals') = sort $ concatMap valuetoKV $ M.toList vals'
  where
    valuetoKV :: (Text, DocValue) -> [([Text], Text)]
    valuetoKV (name, Value str) = [([name], str)]
    valuetoKV (name, Subdocument doc) = addPrefix name $ toKVList doc
    addPrefix :: Text -> [([Text], Text)] -> [([Text], Text)]
    addPrefix prefix = map (\(name,val)->(prefix:name,val))

-- | Convert a 'Document' into a list of key/value pairs.
fromKVList :: [([Text], Text)]
           -> Document
fromKVList [] = Document $ M.fromList []
fromKVList _  = error "fromKVList undefined"

-- | Generate a list of diff operations between two lists of key/value pairs.
diffKVList :: [([Text], Text)] -- ^ Source list.
           -> [([Text], Text)] -- ^ Target list.
           -> [DiffOp ()]
diffKVList _ _ = error "diffKVList undefined"

