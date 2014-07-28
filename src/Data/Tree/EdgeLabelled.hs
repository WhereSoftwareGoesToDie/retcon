module Data.Tree.EdgeLabelled where

import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe

-- | A tree similar to 'Data.Tree' except it uses a 'Map' of children
-- instead a list.
data Tree key value = Node
  { rootLabel    :: Maybe value
  , nodeChildren :: Map key (Tree key value)
  }
  deriving (Eq, Read, Show)

-- | An empty, unlabelled tree.
emptyTree :: Tree a b
emptyTree = Node Nothing M.empty

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
toList = map (\(k,v) -> (reverse k, v)) . worker []
  where
    worker p (Node Nothing  kids) =       children p kids
    worker p (Node (Just v) kids) = (p,v):children p kids
    children p = concatMap (\(l,c) -> worker (l:p) c) . M.toList

-- | Convert an association list of tree paths and values into a 'Tree'.
fromList :: (Ord k)
         => [([k], v)]
         -> Tree k v
fromList [] = emptyTree
fromList ks = foldl (worker) emptyTree ks
  where
    worker (Node l kids) ([]  ,v) = Node (Just v) kids
    worker (Node l kids) (k:ks,v) = Node l $ M.alter (update (ks,v)) k kids
    update vs       Nothing = update vs $ Just emptyTree
    update ([],v)   (Just (Node _ ch)) = Just $ Node (Just v) ch
    update (k:ks,v) (Just (Node l ch)) =
      let emptyCase = update (ks,v) $ Just emptyTree
          fullCase n = update (ks,v) $ Just n
          ch' = M.alter (maybe emptyCase fullCase) k ch
      in Just $ Node l ch'

