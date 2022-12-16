module Day16 (solve) where
import           Data.List   (find)
import           Data.Map    (Map, (!))
import qualified Data.Map    as Map
import           Data.Maybe  (fromJust)
import           Graphs      (distancesBFS)
import           Solution    (Solution (S))
import           StringUtils (getIntegers)

solve :: String -> (Solution, Solution)
solve input = (S (show valves), S "")
  where
    valves = parseInput input

data Valve = Valve{
  vName      :: String,
  vFlow      :: Integer,
  vNeighbors :: [String]
} deriving (Show, Eq, Ord)

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

generateDistances :: [Valve] -> Map String (Map String Integer)
generateDistances valves = Map.fromList [(vName valve, distancesTo valve adjacencyFun nonZeroValves) | valve <- startValve : nonZeroValves]
  where
    startValve = fromJust $ find (\valve -> vName valve == "AA") valves
    nonZeroValves = filter (\valve -> vFlow valve /= 0) valves
    adjacencyFun = getAdjacencyFun valves

distancesTo :: Valve -> (Valve -> [Valve]) -> [Valve] -> Map String Integer
distancesTo startValve adjacencyFun targetValves = undefined
  where
    distances = Map.filterWithKey (\valve _ -> valve `elem` targetValves) (distancesBFS startValve adjacencyFun)
