module Day16 (solve) where
import           Data.List   (delete, find, sort)
import           Data.Map    (Map, (!))
import qualified Data.Map    as Map
import           Data.Maybe  (fromJust)
import           Debug.Trace (trace)
import           Graphs      (distancesBFS)
import           Solution    (Solution (I, S))
import           StringUtils (getIntegers)

solve :: String -> (Solution, Solution)
solve input = (I part1, S "")
  where
    valves = parseInput input
    distanceMap = generateDistances valves
    valveMap = Map.fromList $ map (\valve -> (vName valve, valve)) $ filter (\valve -> vFlow valve /= 0) valves
    part1 = maxFlow distanceMap valveMap

data Valve = Valve{
  vName      :: String,
  vFlow      :: Integer,
  vNeighbors :: [String]
} deriving (Show, Eq, Ord)


type DistanceMap = Map String (Map String Integer)

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

maxFlow :: DistanceMap -> Map String Valve -> Integer
maxFlow = maxFlow' "AA" 30

maxFlow' :: String -> Integer -> DistanceMap -> Map String Valve -> Integer
maxFlow' currentRoom timeLeft distanceMap valveMap
  | null reachableRoutes = 0
  | otherwise = maximum [flowGenerated room travelCost + subFlow room travelCost | (room, travelCost) <- reachableRoutes]
  where
    reachableRoutes = [(room, travelCost) | room <- Map.keys valveMap, let travelCost = (distanceMap ! currentRoom) ! room, travelCost + 1 < timeLeft]
    subFlow room travelCost = maxFlow' room (timeLeft - travelCost - 1) distanceMap (Map.delete room valveMap)
    flowGenerated room travelCost = (timeLeft - travelCost - 1) * vFlow (valveMap ! room)
