{-# LANGUAGE InstanceSigs #-}
module Graphs (shortestPathBFS, shortestPathBFS', nodesFromPath, Edge, Path) where

import           Data.Hashable     (Hashable)
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS

data Pre a = Pre a | End

newtype Edge a = Edge (a, a)

type Path a = [Edge a]

instance (Show a) => Show (Edge a) where
  show :: Edge a -> String
  show (Edge (from, to)) = show from ++ " -> " ++ show to

shortestPathBFS :: (Eq a, Hashable a) => a -> a -> (a -> [a]) -> Maybe (Path a)
shortestPathBFS start = shortestPathBFS' [start]

shortestPathBFS' :: (Eq a, Hashable a) => [a] -> a -> (a -> [a]) -> Maybe (Path a)
shortestPathBFS' startNodes goal adjacencyFun = shortestPathBFSInternal startNodes' startNodes' adjacencyFun goal
  where
    startNodes' = HM.fromList [(start, End) | start <- startNodes]

shortestPathBFSInternal :: (Eq a, Hashable a) => HashMap a (Pre a) -> HashMap a (Pre a) -> (a -> [a]) -> a -> Maybe (Path a)
shortestPathBFSInternal lastLayer visited adjacencyFun goal
  | goal `HM.member` lastLayer = Just $ buildPath visited goal
  | otherwise = if HM.null lastLayer'
    then Nothing
    else shortestPathBFSInternal lastLayer' visited' adjacencyFun goal
  where
    lastLayer' = nextLayer lastLayer visited adjacencyFun
    visited' = HM.union lastLayer' visited

nextLayer :: (Eq a, Hashable a) => HashMap a (Pre a) -> HashMap a (Pre a) -> (a -> [a]) -> HashMap a (Pre a)
nextLayer lastLayer visited adjacencyFun = foldr (HM.union . newNeighbors visited adjacencyFun) HM.empty (HM.keys lastLayer)

newNeighbors :: (Eq a, Hashable a) => HashMap a (Pre a) -> (a -> [a]) -> a -> HashMap a (Pre a)
newNeighbors visited adjacency current = HM.fromList $ [(neighbor, Pre current) | neighbor <- adjacency current, not $ neighbor `HM.member` visited]

buildPath :: (Eq a, Hashable a) => HashMap a (Pre a) -> a -> Path a
buildPath preMap current = case preMap ! current of
  End     -> []
  Pre pre -> Edge (pre, current) : buildPath preMap pre

nodesFromPath :: (Eq a, Hashable a) => Path a -> HashSet a
nodesFromPath = foldr (\(Edge (from, to)) nodes -> HS.insert from (HS.insert to nodes)) HS.empty
