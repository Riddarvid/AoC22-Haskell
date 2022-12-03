module Main (main) where

import           Day3 (solve)

main :: IO ()
main = do
  input <- readFile "app/input/input3.txt"
  let (part1, part2) = solve input
  putStrLn "Part1:"
  print part1
  putStrLn ""
  putStrLn "Part2:"
  print part2
