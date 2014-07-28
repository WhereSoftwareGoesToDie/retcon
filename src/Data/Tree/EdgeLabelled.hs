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

navigate :: Ord k => [k] -> Tree k v -> Maybe v
navigate []     (Node v _kids) = v
navigate (n:ns) (Node _ kids ) = M.lookup n kids >>= navigate ns


