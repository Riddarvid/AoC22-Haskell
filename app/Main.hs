module Main (main) where

import           Day4 (solve)

main :: IO ()
main = do
  input <- readFile "app/input/input4.txt"
  let (part1, part2) = solve input
  putStrLn "Part1:"
  print part1
  putStrLn ""
  putStrLn "Part2:"
  print part2
