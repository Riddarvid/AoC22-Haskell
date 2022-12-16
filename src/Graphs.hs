{-# LANGUAGE InstanceSigs #-}
module Graphs (shortestPathBFS, shortestPathBFS', nodesFromPath, distancesBFS, Edge, Path) where
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

data Pre a = Pre a | End

newtype Edge a = Edge (a, a)

type Path a = [Edge a]

instance (Show a) => Show (Edge a) where
  show :: Edge a -> String
  show (Edge (from, to)) = show from ++ " -> " ++ show to

-- Distance to all

distancesBFS :: (Ord a) => a -> (a -> [a]) -> Map a Integer
distancesBFS start adjacencyFun = distancesBFSInternal (Map.keysSet startMap) startMap adjacencyFun 1
  where
    startMap = Map.singleton start 0

distancesBFSInternal :: (Ord a) => Set a -> Map a Integer -> (a -> [a]) -> Integer -> Map a Integer
distancesBFSInternal lastLayer visited _ _ | null lastLayer = visited
distancesBFSInternal lastLayer visited adjacencyFun layerIndex = distancesBFSInternal (Map.keysSet lastLayer') visited' adjacencyFun (layerIndex + 1)
  where
    lastLayer' = nextLayer lastLayer (Map.keysSet visited) adjacencyFun
    visited' = Map.union (Map.map (const layerIndex) lastLayer') visited

-- Shortest path from start(s) to goal

shortestPathBFS :: (Ord a) => a -> a -> (a -> [a]) -> Maybe (Path a)
shortestPathBFS start = shortestPathBFS' [start]

shortestPathBFS' :: (Ord a) => [a] -> a -> (a -> [a]) -> Maybe (Path a)
shortestPathBFS' startNodes goal adjacencyFun = shortestPathBFSInternal (Map.keysSet startNodes') startNodes' adjacencyFun goal
  where
    startNodes' = Map.fromList [(start, End) | start <- startNodes]

shortestPathBFSInternal :: (Ord a) => Set a -> Map a (Pre a) -> (a -> [a]) -> a -> Maybe (Path a)
shortestPathBFSInternal lastLayer visited adjacencyFun goal
  | goal `elem` lastLayer = Just $ buildPath visited goal
  | otherwise = if null lastLayer'
    then Nothing
    else shortestPathBFSInternal (Map.keysSet lastLayer') visited' adjacencyFun goal
  where
    lastLayer' = nextLayer lastLayer (Map.keysSet visited) adjacencyFun
    visited' = Map.union lastLayer' visited

-- General

nextLayer :: (Ord a, Foldable t) => t a -> t a -> (a -> [a]) -> Map a (Pre a)
nextLayer lastLayer visited adjacencyFun = foldr (Map.union . newNeighbors visited adjacencyFun) Map.empty lastLayer

newNeighbors :: (Ord a, Foldable t) => t a -> (a -> [a]) -> a -> Map a (Pre a)
newNeighbors visited adjacency current = Map.fromList $ [(neighbor, Pre current) | neighbor <- adjacency current, neighbor `notElem` visited]

-- Path building

buildPath :: (Ord a) => Map a (Pre a) -> a -> Path a
buildPath preMap current = case preMap ! current of
  End     -> []
  Pre pre -> Edge (pre, current) : buildPath preMap pre

nodesFromPath :: (Ord a) => Path a -> Set a
nodesFromPath = foldr (\(Edge (from, to)) nodes -> Set.insert from (Set.insert to nodes)) Set.empty
