module Day14 (solve) where
import           Data.List.Utils (split)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Solution        (Solution (I))
import           StringUtils     (getInts)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    edges = parseInput input
    maxY = getMaxY edges

    -- Part 1
    rocks1 = parseRocks edges
    sandMap1 = fill1 rocks1 maxY
    part1 = toInteger $ Set.size sandMap1

    -- Part 2
    floorDepth = maxY + 2
    floorEdge = ((500 - (floorDepth + 10), floorDepth), (500 + floorDepth + 10, floorDepth))
    rocks2 = parseRocks (floorEdge : edges)
    sands2 = fill2 rocks2 (Set.singleton (500, 0)) floorDepth
    part2 = toInteger $ Set.size sands2

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

parseRocks :: [Edge] -> Set Pos
parseRocks = foldr (Set.union . getRockEdge) Set.empty

getRockEdge :: Edge -> Set Pos
getRockEdge ((fromX, fromY), (toX, toY)) = Set.fromList [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
  where
    minX = min fromX toX
    maxX = max fromX toX
    minY = min fromY toY
    maxY = max fromY toY

-- Part 1

fill1 :: Set Pos -> Int -> Set Pos
fill1 rocks = fill1' rocks Set.empty

fill1' :: Set Pos -> Set Pos -> Int -> Set Pos
fill1' rocks sands maxY
  | changed = fill1' rocks sands' maxY
  | otherwise = sands'
  where
    (sands', changed) = addGrain rocks sands maxY

addGrain :: Set Pos -> Set Pos -> Int -> (Set Pos, Bool)
addGrain = addGrain' (500, 0)

addGrain' :: Pos -> Set Pos -> Set Pos -> Int -> (Set Pos, Bool)
addGrain' _ _ sands _ | Set.member (500, 0) sands = (sands, False)
addGrain' (x, y) rocks sands maxY
  | y > maxY = (sands, False)                                   -- Grain is falling forever
  | free down rocks sands = addGrain' down rocks sands maxY    -- Grain falls straight down
  | free left rocks sands = addGrain' left rocks sands maxY    -- Grain falls to the left
  | free right rocks sands = addGrain' right rocks sands maxY  -- Grain falls to the right
  | otherwise = (Set.insert (x, y) sands, True)             -- Grain cannot fall and therefore stops where it is
  where
    down = (x, y + 1)
    left = (x - 1, y + 1)
    right = (x + 1, y + 1)

free :: Pos -> Set Pos -> Set Pos -> Bool
free pos rocks sands = not (Set.member pos rocks) && not (Set.member pos sands)

-- Part2

fill2 :: Set Pos -> Set Pos -> Int -> Set Pos
fill2 rocks lowestLayer maxY
  | Set.size lowestLayer == 0 = lowestLayer
  | otherwise = Set.union lowestLayer $ fill2 rocks lowestLayer' maxY
  where
    lowestLayer' = Set.fold (Set.union . spawnGrains rocks maxY) Set.empty lowestLayer

spawnGrains :: Set Pos -> Int -> Pos -> Set Pos
spawnGrains rocks maxY (x, y)
  | y' > maxY = Set.empty
  | otherwise = Set.filter (`notElem` rocks) $ Set.fromList [(x - 1, y'), (x, y'), (x + 1, y')]
  where
    y' = y + 1
