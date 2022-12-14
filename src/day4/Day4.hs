module Day4 (solve) where

import           Solution    (Solution (I))
import           StringUtils (getIntegers)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    pairs = parsePairs input
    part1 = totalContained pairs
    part2 = totalOverlaps pairs

type Range = (Integer,Integer)

type Pair = (Range, Range)

-- Part 1

totalContained :: [Pair] -> Integer
totalContained pairs = toInteger $ length $ filter pairContained pairs

pairContained :: Pair -> Bool
pairContained (a, b) = (a `containsPair` b) || (b `containsPair` a)

containsPair :: Range -> Range -> Bool
containsPair (low1, high1) (low2, high2) = low1 <= low2 && high1 >= high2

-- Part 2

totalOverlaps :: [Pair] -> Integer
totalOverlaps pairs = toInteger $ length $ filter (uncurry overlaps) pairs

overlaps :: Range -> Range -> Bool
overlaps (low1, high1) (low2, high2) = (high1 >= low2) && (high2 >= low1)

-- Parsing

parsePairs :: String -> [Pair]
parsePairs input = map parsePair $ lines input

parsePair :: String -> Pair
parsePair str = case getIntegers str of
  [low1, high1, low2, high2] -> ((low1, high1), (low2, high2))
  _                          -> error "Parse error"
