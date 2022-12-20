module Day16 (solve) where
import           Control.Monad.State (State, evalState, get, modify)
import           Data.List           (delete, find, permutations, sort)
import           Data.Map            (Map, (!))
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           GHC.Data.Maybe      (isNothing)
import           Graphs              (distancesBFS)
import           Solution            (Solution (I, S))
import           StringUtils         (getIntegers)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    valves = parseInput input
    distanceMap = generateDistances valves
    valveMap = Map.fromList $ map (\valve -> (vName valve, valve)) $ filter (\valve -> vFlow valve /= 0 || vName valve == "AA") valves
    part1 = maxFlow distanceMap valveMap 30
    part2 = maxFlow distanceMap valveMap 26

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


-- General

-- The general solution is based on a recurrence

type FlowMem = (String, Set String, Integer)

type MaxMap = Map FlowMem Integer

maxFlow :: DistanceMap -> ValveMap -> Integer -> Integer
maxFlow distanceMap valveMap time = evalState (maxFlow' distanceMap valveMap (Set.singleton "AA") (Set.delete "AA" $ Map.keysSet valveMap) "AA" time) Map.empty

maxFlow' :: DistanceMap -> ValveMap -> Set String -> Set String -> String -> Integer -> State MaxMap Integer
maxFlow' _ _ _ _ _ timeLeft | timeLeft <= 0 = return 0
maxFlow' distanceMap valveMap visited remaining currentRoom timeLeft = do
  maxMap <- get
  case Map.lookup (currentRoom, visited, timeLeft) maxMap of
    Just flow -> return flow
    Nothing -> do
      subFlows <- mapM (subFlow distanceMap valveMap visited remaining timeLeft currentRoom) (Set.toList remaining)
      let maxSubFlow = foldr (\(_, flow) acc -> max flow acc) 0 subFlows
      let flow = flowGenerated valveMap currentRoom timeLeft + maxSubFlow
      modify (Map.insert (currentRoom, visited, timeLeft) flow) -- Memoization step
      return flow

subFlow :: DistanceMap -> ValveMap -> Set String -> Set String -> Integer -> String -> String -> State MaxMap (FlowMem, Integer)
subFlow distanceMap valveMap visited remaining timeLeft currentRoom nextRoom = do
  flow <- maxFlow' distanceMap valveMap visited' remaining' nextRoom timeLeft'
  return (flowMem, flow)
  where
    visited' = Set.insert nextRoom visited
    remaining' = Set.delete nextRoom remaining
    timeLeft' = spendTime distanceMap currentRoom nextRoom timeLeft
    flowMem = (nextRoom, visited', timeLeft')

spendTime :: DistanceMap -> String -> String -> Integer -> Integer
spendTime distanceMap currentRoom nextRoom timeLeft = timeLeft - ((distanceMap ! currentRoom) ! nextRoom) - 1

-- We already know that timeLeft is > 0
flowGenerated :: ValveMap -> String -> Integer -> Integer
flowGenerated valveMap currentRoom timeLeft = timeLeft * vFlow (valveMap ! currentRoom)
