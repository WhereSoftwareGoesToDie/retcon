--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- |
-- Description: A trie with arbitrary key values.
--
-- This module defines a generic trie-like data structure where keys are
-- sequences of any type in 'Ord'.

{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Tree.GenericTrie where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Prelude hiding (concatMap, foldl, foldr)

-- | A tree similar to 'Data.Tree' except it uses a 'Map' of children
-- instead a list.
data Tree key value
    = Node { nodeValue    :: Maybe value
           , nodeChildren :: Map key (Tree key value)
           }
  deriving (Eq, Read, Show, Functor, Traversable, Foldable)

instance Monoid (Tree a b) where
    mempty = Node Nothing M.empty
    mappend = error "Monoid(Tree a b): union not implemented"

instance FunctorWithIndex [i] (Tree i)
instance FoldableWithIndex [i] (Tree i)
instance TraversableWithIndex [i] (Tree i) where
    itraverse f = go []
      where
        -- go :: [i] -> Tree i a -> f (Tree i a)
        go idx (Node Nothing child_map) =
            Node Nothing <$> itraverse (appendIndex idx) child_map

        go idx (Node (Just v) child_map) =
            Node <$> fmap Just (f idx v)
                 <*> itraverse (appendIndex idx) child_map

        appendIndex idx i = go (idx ++ [i])

instance Plated (Tree k v) where
    plate f (Node v childs) = Node v <$> traverse f childs

-- | Predicate: is Tree empty?
emptyNode :: Tree k v -> Bool
emptyNode (Node l kids) = isNothing l && M.null kids

-- | Filter a Tree by a predicate.
filterTree :: (Tree k v -> Bool) -> Tree k v -> Tree k v
filterTree p = rewrite f
  where
    f t@(Node x childs) =
        if M.null . M.filter emptyNode $ childs
            then if p t then Nothing else Just (Node Nothing childs)
            else Just (Node x (M.filter (not . emptyNode) childs))

-- | Prune a Tree, removing empty nodes.
pruneTree :: Tree k v -> Tree k v
pruneTree = filterTree (not . emptyNode)

-- | Follow a path and return the value, if any, at the end.
navigate :: (Ord k)
         => [k]
         -> Tree k v
         -> Maybe v
navigate []     (Node v _kids) = v
navigate (n:ns) (Node _ kids ) = M.lookup n kids >>= navigate ns

-- | Convert a 'Tree' into an association list of tree paths and values.
toList :: (Ord k)
       => Tree k v
       -> [([k], v)]
toList t = t ^@.. itraversed

-- | Convert an association list of tree paths and values into a 'Tree'.
fromList :: (Ord k)
         => [([k], v)]
         -> Tree k v
fromList [] = mempty
fromList kvs = foldl worker mempty kvs
  where
    worker (Node _ kids) ([]  ,v) = Node (Just v) kids
    worker (Node l kids) (k:ks,v) = Node l $ M.alter (update (ks,v)) k kids
    update vs       Nothing = update vs $ Just mempty
    update ([],v)   (Just (Node _ ch)) = Just $ Node (Just v) ch
    update (k:ks,v) (Just (Node l ch)) =
      let emptyCase = update (ks,v) $ Just mempty
          fullCase n = update (ks,v) $ Just n
          ch' = M.alter (maybe emptyCase fullCase) k ch
      in Just $ Node l ch'
