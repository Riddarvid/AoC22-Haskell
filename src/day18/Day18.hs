module Day18 (solve) where
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Graphs      (reachableBFS)
import           Solution    (Solution (I, S))
import           StringUtils (getIntegersNeg)

solve :: String -> (Solution, Solution)
solve input = (I part1, S $ show part2)
  where
    lavaDrop = parseInput input
    part1 = solvePart1 lavaDrop
    part2 = solvePart2 lavaDrop

type Pos = (Integer, Integer, Integer)

type Drop = Set Pos

-- Parsing

parseInput :: String -> Drop
parseInput input = Set.fromList $ map parsePos $ lines input

parsePos :: String -> Pos
parsePos posStr = case getIntegersNeg posStr of
  [a, b, c] -> (a, b, c)
  _         -> error "Parse error"

-- General

addPos :: Pos -> Pos -> Pos
addPos (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

adjacent :: Pos -> Set Pos
adjacent pos = Set.map (addPos pos) neighbors

neighbors :: Set Pos
neighbors = Set.fromList [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]

-- Part 1

solvePart1 :: Drop -> Integer
solvePart1 lava = Set.fold (\pos acc -> freeSides pos lava + acc) 0 lava


freeSides :: Pos -> Drop -> Integer
freeSides pos lava = toInteger $ Set.size $ Set.filter (`Set.notMember` lava) (adjacent pos)

-- Part 2

solvePart2 :: Drop -> Integer
solvePart2 lava = Set.fold (\pos acc -> freeSides2 pos outerLayer + acc) 0 lava
  where
    dimension = findDimension lava
    outerLayer = findOuterLayer dimension lava

freeSides2 :: Pos -> Set Pos -> Integer
freeSides2 pos outerLayer = toInteger $ Set.size $ Set.filter (`Set.member` outerLayer) (adjacent pos)

-- Find dimensions

findDimension :: Drop -> (Pos, Pos)
findDimension = Set.fold minMax start
  where
    large = 10000000
    small = -10000000
    start = ((large, large, large), (small, small, small))

minMax :: Pos -> (Pos, Pos) -> (Pos, Pos)
minMax (x, y, z) ((minX, minY, minZ), (maxX, maxY, maxZ)) = ((minX', minY', minZ'), (maxX', maxY', maxZ'))
  where
    minX' = min x minX
    minY' = min y minY
    minZ' = min z minZ
    maxX' = max x maxX
    maxY' = max y maxY
    maxZ' = max z maxZ

-- Constructing the outer layer

findOuterLayer :: (Pos, Pos) -> Drop -> Set Pos
findOuterLayer (lowPos, highPos) lava = reachableBFS startNode adjacencyFun
  where
    lowPos' = addPos lowPos (-1, -1, -1)
    highPos' = addPos highPos (1, 1, 1)
    outerDimension = (lowPos', highPos')
    startNode = lowPos'
    adjacencyFun pos = Set.toList $ Set.filter (inOuterLayer lava outerDimension) $ adjacent pos

inOuterLayer :: Drop -> (Pos, Pos) -> Pos -> Bool
inOuterLayer lava dimension pos = (pos `Set.notMember` lava) && inBounds dimension pos

inBounds :: (Pos, Pos) -> Pos -> Bool
inBounds dimension (x, y, z) =
  x >= minX && x <= maxX &&
  y >= minY && y <= maxY &&
  z >= minZ && z <= maxZ
  where
    ((minX, minY, minZ), (maxX, maxY, maxZ)) = dimension
