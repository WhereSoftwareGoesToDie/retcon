module TreeHelpers where

import Control.Applicative
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import ToolShed.Test.QuickCheck.Arbitrary.Map

import Data.Tree.EdgeLabelled

branchingFactor = 8

-- | Generate a random tree with roughtly the given number of nodes.
--
-- This is thorougly biased, but will definitely terminate. Essentially
-- we generate trees according to the size in the following cases:
--
-- - size 1 - make a single leaf node.
-- - size <= 5 - make a node with leafish children.
-- - size > 5 - split it into roughly branchingFactor branches.
generateTree :: (Ord key, Arbitrary key, Arbitrary value)
             => Int
             -> Gen (Tree key value)
generateTree n | n == 0    = pure  $  Node Nothing M.empty
               | n == 1    = Node <$> (Just <$> arbitrary) <*> pure M.empty
               | n <= 5    = Node <$> arbitrary <*> pure M.empty
               | otherwise = do
                    label <- arbitrary
                    let size = n `div` branchingFactor
                    let kids = [1..branchingFactor]
                    nodes <- sequence $ take branchingFactor $ repeat (generateTree size)
                    labels <- sequence $ take branchingFactor $ repeat (arbitrary)

                    return $ Node label (M.fromList $ zip labels nodes)

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Tree k v) where
  arbitrary = sized $ generateTree

