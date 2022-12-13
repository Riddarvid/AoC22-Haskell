{-# LANGUAGE InstanceSigs #-}
module Graphs (shortestPathBFS, Edge, Path) where

import           Data.Hashable     (Hashable)
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM

data Pre a = Pre a | End

newtype Edge a = Edge (a, a)

type Path a = [Edge a]

instance (Show a) => Show (Edge a) where
  show :: Edge a -> String
  show (Edge (from, to)) = show from ++ " -> " ++ show to

shortestPathBFS :: (Eq a, Hashable a) => a -> a -> HashMap a [a] -> Maybe (Path a)
shortestPathBFS start goal adjacency = shortestPathBFS' (HM.singleton start End) (HM.singleton start End) adjacency goal

shortestPathBFS' :: (Eq a, Hashable a) => HashMap a (Pre a) -> HashMap a (Pre a) -> HashMap a [a] -> a -> Maybe (Path a)
shortestPathBFS' lastLayer visited adjacency goal
  | goal `HM.member` lastLayer = Just $ buildPath visited goal
  | otherwise = if HM.null lastLayer'
    then Nothing
    else shortestPathBFS' lastLayer' visited' adjacency goal
  where
    lastLayer' = nextLayer lastLayer visited adjacency
    visited' = HM.union lastLayer' visited

nextLayer :: (Eq a, Hashable a) => HashMap a (Pre a) -> HashMap a (Pre a) -> HashMap a [a] -> HashMap a (Pre a)
nextLayer lastLayer visited adjacency = foldr (HM.union . newNeighbors visited adjacency) HM.empty (HM.keys lastLayer)

newNeighbors :: (Eq a, Hashable a) => HashMap a (Pre a) -> HashMap a [a] -> a -> HashMap a (Pre a)
newNeighbors visited adjacency current = HM.fromList $ [(neighbor, Pre current) | neighbor <- adjacency ! current, not $ neighbor `HM.member` visited]

buildPath :: (Eq a, Hashable a) => HashMap a (Pre a) -> a -> Path a
buildPath preMap current = case preMap ! current of
  End     -> []
  Pre pre -> Edge (pre, current) : buildPath preMap pre
