module Day18 (solve) where
import           Data.Set    (Set)
import qualified Data.Set    as Set
import           Solution    (Solution (I, S))
import           StringUtils (getIntegers, getIntegersNeg)

solve :: String -> (Solution, Solution)
solve input = (I part1, S "")
  where
    lavaDrop = parseInput input
    part1 = solvePart1 lavaDrop

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

-- Part 1

solvePart1 :: Drop -> Integer
solvePart1 lava = Set.fold (\pos acc -> freeSides pos lava + acc) 0 lava

freeSides :: Pos -> Drop -> Integer
freeSides pos lava = toInteger $ Set.size $ Set.filter (`Set.notMember` lava) (adjacent pos)

adjacent :: Pos -> Set Pos
adjacent pos = Set.map (addPos pos) neighbors

neighbors :: Set Pos
neighbors = Set.fromList [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)]
