module Day14 (solve) where
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.List.Utils   (split)
import           Solution          (Solution (I))
import           StringUtils       (getInts)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    edges = parseInput input
    maxY = getMaxY edges
    rockMap1 = getRockMap edges
    sandMap1 = fillWithSand rockMap1 maxY
    part1 = toInteger $ HM.size $ HM.filter (== Sand) sandMap1
    floorDepth = maxY + 2
    rockMap2 = getRockMap (((500 - (floorDepth + 10), floorDepth), (500 + floorDepth + 10, floorDepth)) : edges) -- Add an edge representing the floor
    sandMap2 = fillWithSand rockMap2 floorDepth
    part2 = toInteger $ HM.size $ HM.filter (== Sand) sandMap2

data Filled = Rock | Sand
  deriving (Show, Eq)

type FillMap = HashMap Pos Filled

type Pos = (Int, Int)

type Edge = (Pos, Pos)

-- Parsing

parseInput :: String -> [Edge]
parseInput input = foldr (\formationStr edges -> parseFormation formationStr ++ edges) [] $ lines input

parseFormation :: String -> [Edge]
parseFormation str = parseEdges $ split " -> " str

parseEdges :: [String] -> [Edge]
parseEdges (from : to : str) = case getInts from of
  [fromX, fromY] -> case getInts to of
    [toX, toY] -> ((fromX, fromY), (toX, toY)) : parseEdges (to : str)
    _          -> error "Parse error"
  _ -> error "Parse error"
parseEdges _ = []

-- Utils

getMaxY :: [Edge] -> Int
getMaxY []                            = -1000
getMaxY (((_, fromY), (_, toY)) : xs) = maximum [fromY, toY, getMaxY xs]

getRockMap :: [Edge] -> FillMap
getRockMap edges = HM.fromList $ foldr (\edge acc -> getRockEdge edge ++ acc) [] edges

getRockEdge :: Edge -> [(Pos, Filled)]
getRockEdge ((fromX, fromY), (toX, toY)) = [((x, y), Rock) | x <- [minX .. maxX], y <- [minY .. maxY]]
  where
    minX = min fromX toX
    maxX = max fromX toX
    minY = min fromY toY
    maxY = max fromY toY

-- Part 1

fillWithSand :: FillMap -> Int -> FillMap
fillWithSand fillMap maxY
  | changed = fillWithSand fillMap' maxY
  | otherwise = fillMap'
  where
    (fillMap', changed) = addGrain fillMap maxY

addGrain :: FillMap -> Int -> (FillMap, Bool)
addGrain = addGrain' (500, 0)

addGrain' :: Pos -> FillMap -> Int -> (FillMap, Bool)
addGrain' _ fillMap _ | HM.member (500, 0) fillMap = (fillMap, False)
addGrain' (x, y) fillMap maxY
  | y > maxY = (fillMap, False)                                   -- Grain is falling forever
  | not $ HM.member down fillMap = addGrain' down fillMap maxY    -- Grain falls straight down
  | not $ HM.member left fillMap = addGrain' left fillMap maxY    -- Grain falls to the left
  | not $ HM.member right fillMap = addGrain' right fillMap maxY  -- Grain falls to the right
  | otherwise = (HM.insert (x, y) Sand fillMap, True)             -- Grain cannot fall and therefore stops where it is
  where
    down = (x, y + 1)
    left = (x - 1, y + 1)
    right = (x + 1, y + 1)
