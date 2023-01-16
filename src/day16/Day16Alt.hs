module Day16Alt (solve) where
import           Data.List   (find, maximumBy)
import           Data.Map    (Map, (!), (!?))
import qualified Data.Map    as Map
import           Data.Maybe  (fromJust)
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Debug.Trace (trace)
import           Graphs      (BFSOptions (..), distancesBFS, reachableBFS)
import           Solution    (Solution (I))
import           StringUtils (getIntegers)
import           Utils       (showMap)

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
distancesTo startValve adjacencyFun = Map.mapKeys vName $ Map.filterWithKey (\valve _ -> vFlow valve /= 0) (distancesBFS startValve adjacencyFun options)
  where
    options = BFSOptions{pruneFun = Nothing, keepVisited = True}

-- Part 1

solvePart1 :: DistanceMap -> ValveMap -> Integer
solvePart1 = maxFlow (Map.singleton ("AA", 30) 1)

-- Part 2

solvePart2 :: DistanceMap -> ValveMap -> Integer
solvePart2 = maxFlow (Map.singleton ("AA", 26) 2)

-- General

data State = State{
  flowGenerated :: Integer,
  visited       :: Set String,
  current       :: Map (String, Integer) Integer
} deriving (Eq, Ord, Show)

compareFlow :: State -> State -> Ordering
compareFlow a b = compare (flowGenerated a) (flowGenerated b)

maxFlow :: Map (String, Integer) Integer -> DistanceMap -> ValveMap -> Integer
maxFlow startPositions distanceMap valveMap = trace ("End state: " ++ show maxState ++ "\nDistances: " ++ showMap distanceMap ++ "\nValves: " ++ showMap valveMap)
  flowGenerated maxState
  where
    startState = State {flowGenerated = 0, visited = Set.singleton "AA", current = startPositions}
    options = BFSOptions{pruneFun = Just $ prune valveMap, keepVisited = True}
    endStates = reachableBFS startState (adjacency distanceMap valveMap) options
    maxState = maximumBy (\a b -> compare (flowGenerated a) (flowGenerated b)) endStates

-- Adjacency

adjacency :: DistanceMap -> ValveMap -> State -> [State]
adjacency distanceMap valveMap state =
  filter (\s -> all (\(_, time) -> time >= 0) (Map.keys $ current s)) $ concatMap (nextStates distanceMap valveMap state) $ Map.keys $ current state

nextStates :: DistanceMap -> ValveMap -> State -> (String, Integer) -> [State]
nextStates distanceMap valveMap state (from, timeLeft) = Set.toList $ Set.map (move distanceMap valveMap state timeLeft from) remaining'
  where
    remaining' = remaining valveMap state

move :: DistanceMap -> ValveMap -> State -> Integer -> String -> String -> State
move distanceMap valveMap state timeLeft from to =
  state{flowGenerated = flowGenerated state + newFlow, visited = Set.insert to $ visited state, current = current''}
  where
    (newFlow, newTimeLeft) = calculateNewFlow distanceMap valveMap timeLeft from to
    current' = removeCurrent (current state) (from, timeLeft)
    current'' = addCurrent current' (to, newTimeLeft)

calculateNewFlow :: DistanceMap -> ValveMap -> Integer -> String -> String -> (Integer, Integer)
calculateNewFlow distanceMap valveMap timeLeft from to = (newFlow, newTimeLeft)
  where
    distance = (distanceMap ! from) ! to
    newTimeLeft = timeLeft - (distance + 1)
    newFlow = vFlow (valveMap ! to) * newTimeLeft

removeCurrent :: Map (String, Integer) Integer -> (String, Integer) -> Map (String, Integer) Integer
removeCurrent current' entry = case current' ! entry of
  1 -> Map.delete entry current'
  n -> Map.insert entry (n - 1) current'

addCurrent :: Map (String, Integer) Integer -> (String, Integer) -> Map (String, Integer) Integer
addCurrent current' entry = case current' !? entry of
  Nothing -> Map.insert entry 1 current'
  Just n  -> Map.insert entry (n + 1) current'

-- Pruning

prune :: ValveMap -> Set State -> Set State
prune valveMap states = trace ("Before: " ++ show (Set.size states) ++ " After: " ++ show (Set.size pruned)) pruned
  where
    maxGuaranteed = flowGenerated $ maximumBy compareFlow states
    pruned = Set.filter (\s -> maxPotential valveMap s >= maxGuaranteed) states

-- Our goal here is to create as small of an upper bound as possible
-- We have chosen the total flow of the currently unvisited nodes multiplied by the maximum time remaining
-- It is obvious that this is an upper bound and it is hopefully small enough.
maxPotential :: ValveMap -> State -> Integer
maxPotential valveMap state = flowGenerated state + sum (Set.map (\room -> vFlow (valveMap ! room) * maxTimeLeft) $ remaining valveMap state)
  where
    maxTimeLeft = maximum $ map snd $ Map.keys $ current state

-- Utils

remaining :: ValveMap -> State -> Set String
remaining valveMap state = Set.difference (Map.keysSet valveMap) (visited state)
