{-# LANGUAGE InstanceSigs #-}
module Graphs (shortestPathBFS, shortestPathBFS', nodesFromPath, distancesBFS, reachableBFS, Edge, Path, BFSOptions (BFSOptions, pruneFun, keepVisited)) where
import           Data.Foldable (toList)
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Debug.Trace   (trace)

data Pre a = Pre a | End

newtype Edge a = Edge (a, a)

type Path a = [Edge a]

type AdjacencyFun a = a -> [a]

instance (Show a) => Show (Edge a) where
  show :: Edge a -> String
  show (Edge (from, to)) = show from ++ " -> " ++ show to

-- General BFS

data BFSOptions a = BFSOptions{
  pruneFun    :: Maybe (Set a -> Set a),
  keepVisited :: Bool
}

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

stepBFS :: (Ord a) => BFSOptions a -> BFSState a -> BFSState a
--stepBFS _ state | trace (show (layerIndex state) ++ " Last layer: " ++ show (Set.size (lastLayer state))) False = undefined
stepBFS options state = state{lastLayer = Map.keysSet pruned, visited = visited', layerIndex = layerIndex'}
  where
    lastLayer' = nextLayer state
    pruned = case pruneFun options of
      Nothing -> lastLayer'
      Just pf -> prune lastLayer' pf
    visited'
      | keepVisited options = Map.union lastLayer' (visited state)
      | Map.size pruned == 0 = visited state
      | otherwise = pruned
    layerIndex' = layerIndex state + 1

prune :: Ord a => Map a (Pre a, Integer) -> (Set a -> Set a) -> Map a (Pre a, Integer)
prune lastLayer' pf = Map.filterWithKey (\k _ -> Set.member k pruned) lastLayer'
  where
    pruned = pf $ Map.keysSet lastLayer'

nextLayer :: (Ord a) => BFSState a -> Map a (Pre a, Integer)
nextLayer state = foldr (Map.union . newNeighbors state) Map.empty (lastLayer state)

newNeighbors :: (Ord a) => BFSState a -> a -> Map a (Pre a, Integer)
newNeighbors state current = Map.fromList $ [(neighbor, (Pre current, layerIndex state)) | neighbor <- adjacency current, neighbor `Map.notMember` visited state]
  where
    adjacency = adjacencyFun state

-- Explores one step at a time until there are no more reachable nodes
exploreFully :: (Ord a) => BFSOptions a -> BFSState a -> BFSState a
exploreFully options = until (Set.null . lastLayer) (stepBFS options)

exploreUntil :: (Ord a) => BFSOptions a -> (BFSState a -> Bool) -> BFSState a -> Maybe (BFSState a)
exploreUntil options cond startState
  | cond endState = Just endState
  | otherwise = Nothing
  where
    endState = until (\state -> cond state || Set.null (lastLayer state)) (stepBFS options) startState

-- Distance to all

distancesBFS :: (Ord a) => a -> (a -> [a]) -> BFSOptions a -> Map a Integer
distancesBFS startNode adjacency options = Map.map snd (visited explored)
  where
    startState = startStateSingle startNode adjacency
    explored = exploreFully options startState

-- Shortest path from start(s) to goal

shortestPathBFS :: (Ord a) => a -> a -> AdjacencyFun a -> BFSOptions a -> Maybe (Path a)
shortestPathBFS start = shortestPathBFS' [start]

shortestPathBFS' :: (Ord a) => [a] -> a -> AdjacencyFun a -> BFSOptions a -> Maybe (Path a)
shortestPathBFS' startNodes goal adjacency options = case endState of
  Nothing    -> Nothing
  Just state -> Just $ buildPath state goal
  where
    startState = startStateMultiple startNodes adjacency
    cond state = goal `elem` lastLayer state
    endState = exploreUntil options cond startState

-- Path building

buildPath :: (Ord a) => BFSState a -> a -> Path a
buildPath state current = case visited state ! current of
  (End, _)     -> []
  (Pre pre, _) -> Edge (pre, current) : buildPath state pre

nodesFromPath :: (Ord a) => Path a -> Set a
nodesFromPath = foldr (\(Edge (from, to)) nodes -> Set.insert from (Set.insert to nodes)) Set.empty

-- Find all reachable nodes

reachableBFS :: (Ord a) => a -> AdjacencyFun a -> BFSOptions a -> Set a
reachableBFS start adjacency options = Map.keysSet $ visited endState
  where
    startState = startStateSingle start adjacency
    endState = exploreFully options startState
