module Main (main) where

import qualified Day1     as D1
import qualified Day10    as D10
import qualified Day11    as D11
import qualified Day12    as D12
import qualified Day13    as D13
import qualified Day14    as D14
import qualified Day15    as D15
import qualified Day16Alt as D16
import qualified Day17    as D17
import qualified Day18    as D18
import qualified Day19Alt as D19
import qualified Day2     as D2
import qualified Day20    as D20
import qualified Day21    as D21
import qualified Day22    as D22
import qualified Day23    as D23
import qualified Day3     as D3
import qualified Day4     as D4
import qualified Day5     as D5
import qualified Day6     as D6
import qualified Day7     as D7
import qualified Day8     as D8
import qualified Day9     as D9

import           Solution (Solution)

day :: Int
day = 23

main :: IO ()
main = do
  input <- readFile ("app/input/input" ++ show day ++ ".txt")
  let solution = (solvers !! (day - 1)) input
  printSolution solution

solvers :: [String -> (Solution, Solution)]
solvers = [D1.solve, D2.solve, D3.solve, D4.solve, D5.solve, D6.solve, D7.solve, D8.solve, D9.solve, D10.solve, D11.solve, D12.solve,
  D13.solve, D14.solve, D15.solve, D16.solve, D17.solve, D18.solve, D19.solve, D20.solve, D21.solve, D22.solve, D23.solve]

printSolution :: (Show a, Show b) => (a, b) -> IO ()
printSolution (part1, part2) = do
  putStrLn "Part1:"
  print part1
  putStrLn ""
  putStrLn "Part2:"
  print part2
