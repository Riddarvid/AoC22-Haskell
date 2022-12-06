module Day6 (solve) where

import           Data.List.Utils (uniq)
import           Solution        (Solution (I))

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    part1 = firstNDistinct input 4
    part2 = firstNDistinct input 14

firstNDistinct :: String -> Int -> Integer
firstNDistinct input n
  | length (uniq (take n input)) == n = toInteger n
  | otherwise = 1 + firstNDistinct (tail input) n
