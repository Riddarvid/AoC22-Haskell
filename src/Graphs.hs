{-# LANGUAGE InstanceSigs #-}
module Graphs (shortestPathBFS, shortestPathBFS', nodesFromPath, distancesBFS, reachableBFS, Edge, Path) where
import           Data.Foldable (toList)
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set

data Pre a = Pre a | End

newtype Edge a = Edge (a, a)

type Path a = [Edge a]

type AdjacencyFun a = a -> [a]

instance (Show a) => Show (Edge a) where
  show :: Edge a -> String
  show (Edge (from, to)) = show from ++ " -> " ++ show to

-- General BFS

data BFSState a = BFSState{
  lastLayer    :: Set a,
  visited      :: Map a (Pre a, Integer),-- Predecessor and layer index
  layerIndex   :: Integer,
  adjacencyFun :: a -> [a]
}

startStateSingle :: (Ord a) => a -> AdjacencyFun a -> BFSState a
startStateSingle startNode = startStateMultiple [startNode]

startStateMultiple :: (Ord a, Foldable t) => t a -> AdjacencyFun a -> BFSState a
startStateMultiple startNodes adjacency = BFSState {lastLayer = lastLayer', visited = visited', layerIndex = 0, adjacencyFun = adjacency}
  where
    lastLayer' = Set.fromList $ toList startNodes
    visited' = Map.fromSet (const (End, 0)) lastLayer'

stepBFS :: (Ord a) => BFSState a -> BFSState a
stepBFS state = state{lastLayer = Map.keysSet lastLayer', visited = visited', layerIndex = layerIndex'}
  where
    lastLayer' = nextLayer state
    visited' = Map.union lastLayer' (visited state)
    layerIndex' = layerIndex state + 1

nextLayer :: (Ord a) => BFSState a -> Map a (Pre a, Integer)
nextLayer state = foldr (Map.union . newNeighbors state) Map.empty (lastLayer state)

newNeighbors :: (Ord a) => BFSState a -> a -> Map a (Pre a, Integer)
newNeighbors state current = Map.fromList $ [(neighbor, (Pre current, layerIndex state)) | neighbor <- adjacency current, neighbor `Map.notMember` visited state]
  where
    adjacency = adjacencyFun state

-- Explores one step at a time until there are no more reachable nodes
exploreFully :: (Ord a) => BFSState a -> BFSState a
exploreFully = until (Set.null . lastLayer) stepBFS

exploreUntil :: (Ord a) => (BFSState a -> Bool) -> BFSState a -> Maybe (BFSState a)
exploreUntil cond startState
  | cond endState = Just endState
  | otherwise = Nothing
  where
    endState = until (\state -> cond state || Set.null (lastLayer state)) stepBFS startState

-- Distance to all

distancesBFS :: (Ord a) => a -> (a -> [a]) -> Map a Integer
distancesBFS startNode adjacency = Map.map snd (visited explored)
  where
    startState = startStateSingle startNode adjacency
    explored = exploreFully startState

-- Shortest path from start(s) to goal

shortestPathBFS :: (Ord a) => a -> a -> (a -> [a]) -> Maybe (Path a)
shortestPathBFS start = shortestPathBFS' [start]

shortestPathBFS' :: (Ord a) => [a] -> a -> (a -> [a]) -> Maybe (Path a)
shortestPathBFS' startNodes goal adjacency = case endState of
  Nothing    -> Nothing
  Just state -> Just $ buildPath state goal
  where
    startState = startStateMultiple startNodes adjacency
    cond state = goal `elem` lastLayer state
    endState = exploreUntil cond startState

-- Path building

buildPath :: (Ord a) => BFSState a -> a -> Path a
buildPath state current = case visited state ! current of
  (End, _)     -> []
  (Pre pre, _) -> Edge (pre, current) : buildPath state pre

nodesFromPath :: (Ord a) => Path a -> Set a
nodesFromPath = foldr (\(Edge (from, to)) nodes -> Set.insert from (Set.insert to nodes)) Set.empty

-- Find all reachable nodes
-- TODO refactor so that I use a simple iterating thing for the actual BFS, which I can then use to find reachable, shortest, all etc.

reachableBFS :: (Ord a) => a -> (a -> [a]) -> Set a
reachableBFS start adjacencyFun = undefined
