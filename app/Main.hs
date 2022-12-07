module Main (main) where

import qualified Day1     as D1
import qualified Day2     as D2
import qualified Day3     as D3
import qualified Day4     as D4
import qualified Day5     as D5
import qualified Day6     as D6
import qualified Day7     as D7
import           Solution

day :: Int
day = 7

main :: IO ()
main = do
  input <- readFile ("app/input/input" ++ show day ++ ".txt")
  --let solution = (solvers !! (day - 1)) input
  --printSolution solution
  let solution = D7.solve input
  print solution

solvers :: [String -> (Solution, Solution)]
solvers = [D1.solve, D2.solve, D3.solve, D4.solve, D5.solve, D6.solve]

printSolution :: (Show a, Show b) => (a, b) -> IO ()
printSolution (part1, part2) = do
  putStrLn "Part1:"
  print part1
  putStrLn ""
  putStrLn "Part2:"
  print part2
