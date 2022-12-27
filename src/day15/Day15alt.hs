module Day15alt (solve, testBlock, splitBlock) where
import           Data.List       (find, partition)
import           Data.List.Utils (uniq)
import           Data.Maybe      (fromJust)
import           Debug.Trace     (trace)
import           Solution        (Solution (I))
import           StringUtils     (getIntegersNeg)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    readings = parseInput input

    -- Part 1
    part1 = coveredInRow 2000000 readings

    -- Part 2
    scanners = map (\(Reading pos beacon) -> Scanner pos (distance pos beacon)) readings
    startBlock = Block {botLeft=(0, 0), botRight=(4000000, 0), topLeft=(0, 4000000), topRight=(4000000, 4000000)}
    (x, y) = head $ findNotCovered scanners startBlock
    part2 = x * 4000000 + y

data Block = Block{
  botLeft  :: Pos,
  botRight :: Pos,
  topLeft  :: Pos,
  topRight :: Pos
} deriving Show

blockPoints :: Block -> [Pos]
blockPoints block = [topLeft block, topRight block, botLeft block, botRight block]

type Pos = (Integer, Integer)

data Scanner = Scanner Pos Integer

data Reading = Reading Pos Pos
  deriving Show

-- Parsing

parseInput :: String -> [Reading]
parseInput input = map parseReading $ lines input

parseReading :: String -> Reading
parseReading str = case getIntegersNeg str of
  [sensorX, sensorY, beaconX, beaconY] -> Reading (sensorX, sensorY) (beaconX, beaconY)
  _ -> error "Parse error"

-- Part 1

type Range = (Integer, Integer)

coveredInRow :: Integer -> [Reading] -> Integer
coveredInRow row readings = totalCovered ranges - beaconsCovered row ranges readings
  where
    ranges = rangesAtRow row readings

beaconsCovered :: Integer -> [Range] -> [Reading] -> Integer
beaconsCovered row ranges readings = toInteger $ length $ filter (beaconCovered row ranges) beacons
  where
    beacons = uniq $ map (\(Reading _ beacon) -> beacon) readings

beaconCovered :: Integer -> [Range] -> Pos -> Bool
beaconCovered row _ (_, bY) | row /= bY = False
beaconCovered _ ranges (bX, _) = any (\(low, high) -> bX >= low && bX <= high) ranges

-- Forming ranges

rangesAtRow :: Integer -> [Reading] -> [Range]
rangesAtRow row readings = mergeRanges $ map (rangeAtRow row) readings'
  where
    readings' = filter (coversRow row) readings

rangeAtRow :: Integer -> Reading -> Range
rangeAtRow row reading@(Reading (sX, sY) _) = (sX - dX, sX + dX)
  where
    dY = abs (row - sY)
    dX = radiusByReading reading - dY

coversRow :: Integer -> Reading -> Bool
coversRow row reading@(Reading (_, sY) _) = radiusByReading reading >= abs (row - sY)

radiusByReading :: Reading -> Integer
radiusByReading (Reading (sX, sY) (bX, bY)) = abs (sX - bX) + abs (sY - bY)

-- Merging ranges

totalCovered :: [Range] -> Integer
totalCovered = foldr (\(from, to) acc -> acc + (to - from + 1)) 0

mergeRanges :: [Range] -> [Range]
mergeRanges = foldr mergeRange []

mergeRange :: Range -> [Range] -> [Range]
mergeRange range merged = mergeOverlapping (range : overlapping) : notOverlapping
  where
    (overlapping, notOverlapping) = partition (overlaps range) merged

mergeOverlapping :: [Range] -> Range
mergeOverlapping ranges = (lowest, highest)
  where
    lowest = minimum $ map fst ranges
    highest = maximum $ map snd ranges

overlaps :: Range -> Range -> Bool
overlaps (low1, high1) (low2, high2) = low1 <= high2 && high1 >= low2

-- Part 2

distance :: Pos -> Pos -> Integer
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- Two base cases:
-- 1: The block is completely covered by any scanner - no solution found
-- 2: The block is of size 1 and is not covered by any scanner - solution found
findNotCovered :: [Scanner] -> Block -> [Pos]
findNotCovered scanners block
  | any (block `allCoveredBy`) scanners = [] -- Base case 1
  | blockSize block == 1 = possBlock block
  | otherwise = concatMap (findNotCovered scanners) $ splitBlock block

allCoveredBy :: Block -> Scanner -> Bool
allCoveredBy block scanner = all (`coveredBy` scanner) (blockPoints block)

coveredBy :: Pos -> Scanner -> Bool
coveredBy pointPos (Scanner scanPos radius) = distance pointPos scanPos <= radius

dimensions :: Block -> (Integer, Integer, Integer, Integer)
dimensions block = (minX, maxX, minY, maxY)
  where
    (minX, minY) = botLeft block
    (maxX, maxY) = topRight block

possBlock :: Block -> [Pos]
possBlock block = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
  where
    (minX, maxX, minY, maxY) = dimensions block

blockSize :: Block -> Integer
blockSize block = (maxX - minX + 1) * (maxY - minY + 1)
  where
    (minX, maxX, minY, maxY) = dimensions block

splitBlock :: Block -> [Block]
splitBlock block = [
  Block {botLeft=(minX, minY), botRight=(xDiff, minY), topLeft=(minX, yDiff), topRight=(xDiff, yDiff)}, -- Bottom left
  Block {botLeft=(xDiff + 1, minY), botRight=(maxX, minY), topLeft=(xDiff + 1, yDiff), topRight=(maxX, yDiff)}, -- Bottom right
  Block {botLeft=(minX, yDiff + 1), botRight=(xDiff, yDiff + 1), topLeft=(minX, maxY), topRight=(xDiff, maxY)}, -- Top left
  Block {botLeft=(xDiff + 1, yDiff + 1), botRight=(maxX, yDiff + 1), topLeft=(xDiff + 1, maxY), topRight=(maxX, maxY)}] -- Top right
  where
    (minX, maxX, minY, maxY) = dimensions block
    xDiff = (maxX - minX) `div` 2
    yDiff = (maxY - minY) `div` 2

testBlock :: Block
testBlock = Block {botLeft=(0, 0), botRight=(3, 0), topLeft=(0, 3), topRight=(3, 3)}
