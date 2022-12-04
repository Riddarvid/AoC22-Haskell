module Day4 (solve) where

import           Data.List.Utils (split)

solve :: String -> (Integer, Integer)
solve input = (part1, part2)
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
parsePair str = case split "," str of
  [a, b] -> (parseRange a, parseRange b)
  _      -> error "Invalid pair string"

parseRange :: String -> Range
parseRange str = case split "-" str of
  [a, b] -> (read a, read b)
  _      -> error "Invalid range string"
