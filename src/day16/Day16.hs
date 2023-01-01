{-# LANGUAGE TupleSections #-}
module Day16 (solve) where
import           Data.List   (find, maximumBy)
import           Data.Map    (Map, (!))
import qualified Data.Map    as Map
import           Data.Maybe  (fromJust)
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Graphs      (distancesBFS)
import           Solution    (Solution (I))
import           StringUtils (getIntegers)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    valves = parseInput input
    distanceMap = generateDistances valves
    valveMap = Map.fromList $ map (\valve -> (vName valve, valve)) $ filter (\valve -> vFlow valve /= 0 || vName valve == "AA") valves
    part1 = solvePart1 distanceMap valveMap
    part2 = solvePart2 distanceMap valveMap

data Valve = Valve{
  vName      :: String,
  vFlow      :: Integer,
  vNeighbors :: [String]
} deriving (Show, Eq, Ord)


type DistanceMap = Map String (Map String Integer)

type ValveMap = Map String Valve

-- Parsing

parseInput :: String -> [Valve]
parseInput input = map parseValve $ lines input

parseValve :: String -> Valve
parseValve str = Valve roomId rate roomIds
  where
    tokens = words str
    roomId = tokens !! 1
    rate = head $ getIntegers str
    roomIds = map (take 2) $ drop 9 tokens

-- Setup / graph building

getAdjacencyFun :: [Valve] -> Valve -> [Valve]
getAdjacencyFun valves valve = [valveMap ! neighbor | neighbor <- vNeighbors valve]
  where
    valveMap = Map.fromList [(vName valve', valve') | valve' <- valves]

generateDistances :: [Valve] -> DistanceMap
generateDistances valves = Map.fromList [(vName valve, distancesTo valve adjacencyFun) | valve <- startValve : nonZeroValves]
  where
    startValve = fromJust $ find (\valve -> vName valve == "AA") valves
    nonZeroValves = filter (\valve -> vFlow valve /= 0) valves
    adjacencyFun = getAdjacencyFun valves

distancesTo :: Valve -> (Valve -> [Valve]) -> Map String Integer
distancesTo startValve adjacencyFun = Map.mapKeys vName $ Map.filterWithKey (\valve _ -> vFlow valve /= 0) (distancesBFS startValve adjacencyFun)

-- Part 1

solvePart1 :: DistanceMap -> ValveMap -> Integer
solvePart1 distanceMap valveMap = maximum $ Set.map (flow valveMap) paths
  where
    rooms = Map.keysSet $ Map.delete "AA" valveMap
    paths = generatePaths Simple distanceMap rooms "AA" 30

-- Part 2
-- Process:
-- 1. Find all flows with corresponding paths
-- 2. Group the paths so that for each set of nodes, we have a flow. That is, for all paths visiting the same nodes, find the max flow.
-- 3. For all elements in this new map, find the largest flow for a complementing path.
-- 4. Find the maximum of these.

solvePart2 :: DistanceMap -> Map String Valve -> Integer
solvePart2 distanceMap valveMap = maximum $ Set.map (flowByMatching flowsByNodes) matchings
  where
    rooms = Map.keysSet $ Map.delete "AA" valveMap
    paths = generatePaths Full distanceMap rooms "AA" 26
    flowsByPaths = Map.fromSet (flow valveMap) paths
    flowsByNodes = Map.mapKeysWith max pathToNodes flowsByPaths
    matchings = generateMatchings $ Map.keysSet flowsByNodes

pathToNodes :: Path -> Set String
pathToNodes path = Set.delete "AA" $ Set.fromList $ map fst path -- Delete AA to make checking for complements easier

type Matching = (Set String, Set String)

generateMatchings :: Set (Set String) -> Set Matching
generateMatchings nodeSets = Set.unions $ Set.map (getMatchings nodeSets) nodeSets

getMatchings :: Set (Set String) -> Set String -> Set Matching
getMatchings nodeSets currentSet = Set.map (currentSet,) $ Set.filter (complementing currentSet) nodeSets

complementing :: Set String -> Set String -> Bool
complementing set1 set2 = Set.size (Set.intersection set1 set2) == 0

flowByMatching :: Map (Set String) Integer -> Matching -> Integer
flowByMatching flowMap (nodesA, nodesB) = (flowMap ! nodesA) + (flowMap ! nodesB)

-- General

type Path = [(String, Integer)]

data Mode = Simple | Full

generatePaths :: Mode -> DistanceMap -> Set String -> String -> Integer -> Set Path
generatePaths _ _ _ _ timeLeft | timeLeft < 0 = Set.singleton []
generatePaths _ _ remaining startRoom timeLeft | Set.size remaining == 0 = Set.singleton [(startRoom, timeLeft)]
generatePaths mode distanceMap remaining startRoom timeLeft = case mode of
  Simple -> paths
  Full   -> Set.insert [(startRoom, timeLeft)] paths
  where
    subPaths = Set.unions $ Set.map (subPathsByNext mode distanceMap remaining startRoom timeLeft) remaining
    paths = Set.map ((startRoom, timeLeft) :) subPaths

subPathsByNext :: Mode ->  DistanceMap -> Set String -> String -> Integer -> String -> Set Path
subPathsByNext mode distanceMap remaining room timeLeft nextRoom = generatePaths mode distanceMap remaining' nextRoom timeLeft'
  where
    remaining' = Set.delete nextRoom remaining
    timeLeft' = spendTime distanceMap room nextRoom timeLeft

spendTime :: DistanceMap -> String -> String -> Integer -> Integer
spendTime distanceMap room nextRoom timeLeft = timeLeft - (travelTime + 1)
  where
    travelTime = (distanceMap ! room) ! nextRoom

flow :: ValveMap -> Path -> Integer
flow valveMap = foldr (\(room, timeLeft) acc -> timeLeft * vFlow (valveMap ! room) + acc) 0
