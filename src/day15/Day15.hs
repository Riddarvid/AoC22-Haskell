module Day15 (solve) where
import           Data.List       (find, partition)
import           Data.List.Utils (uniq)
import           Data.Maybe      (fromJust)
import           Debug.Trace     (trace)
import           Solution        (Solution (I))
import           StringUtils     (getIntegersNeg)

solve :: String -> (Solution, Solution)
solve input = (I part1, I $ trace (show (x, y)) part2)
  where
    readings = parseInput input
    part1 = coveredInRow 2000000 readings
    (x, y) = findNotCovered readings 0 4000000
    part2 = x * 4000000 + y

type Pos = (Integer, Integer)

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
    dX = radius reading - dY

coversRow :: Integer -> Reading -> Bool
coversRow row reading@(Reading (_, sY) _) = radius reading >= abs (row - sY)

radius :: Reading -> Integer
radius (Reading (sX, sY) (bX, bY)) = abs (sX - bX) + abs (sY - bY)

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

containsCompletely :: Range -> [Range] -> Bool
containsCompletely (low, high) = any (\(low', high') -> low >= low' && high <= high')

-- Part2

findNotCovered :: [Reading] -> Integer -> Integer -> Pos
findNotCovered readings minVal maxVal = trace (show ranges) (high + 1, row)
  where
    (row, ranges) = fromJust $ find (\(_, ranges') -> not $ containsCompletely (minVal, maxVal) ranges') $ [(row', rangesAtRow row' readings) | row' <- [minVal .. maxVal]]
    (_, high) = fromJust $ find (\(_, high') -> high' >= minVal - 1 && high' < maxVal) ranges
