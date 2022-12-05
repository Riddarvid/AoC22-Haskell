module Day5 (solve) where

import           Data.Char       (isDigit)
import           Data.IntMap     (IntMap, (!))
import qualified Data.IntMap     as IntMap
import           Data.List       (transpose)
import           Data.List.Extra (trimStart)
import           StringUtils     (getInts)

solve :: String -> (String, String)
solve input = (part1, part2)
  where
    (stacks, moves) = parseInput input
    part1 = topCrates $ executeMoves stacks moves move9000
    part2 = topCrates $ executeMoves stacks moves move9001

-- Parsing

type Stacks = IntMap [Char]

data Instruction = Move Int Int Int
  deriving (Show)

parseInput :: String -> (Stacks, [Instruction])
parseInput input = (parseStack stackLines, parseMoves (tail instrLines))
  where
    (stackLines, instrLines) = span (/= "") (lines input)

parseStack :: [String] -> Stacks
parseStack input = IntMap.fromDistinctAscList $ zip [1..] $ map (trimStart . init) $ filter (any isDigit) $ transpose input

parseMoves :: [String] -> [Instruction]
parseMoves = map parseMove
  where
    parseMove :: String -> Instruction
    parseMove moveLine = case map fromInteger (getInts moveLine) of
      [n, from, to] -> Move n from to
      _             -> error "Parse error"

-- Part 1

move9000 :: Int -> [Char] -> [Char] -> ([Char], [Char])
move9000 0 fromStack toStack = (fromStack, toStack)
move9000 n fromStack toStack = move9000 (n-1) (tail fromStack) (head fromStack : toStack)

-- Part 2

move9001 :: Int -> [Char] -> [Char] -> ([Char], [Char])
move9001 n fromStack toStack = (drop n fromStack, take n fromStack ++ toStack)

-- Common

executeMoves :: Stacks -> [Instruction] -> (Int -> [Char] -> [Char] -> ([Char], [Char])) -> Stacks
executeMoves stack instrs moveFun = foldl (\stack' instr -> executeMove stack' instr moveFun) stack instrs

executeMove :: Stacks -> Instruction -> (Int -> [Char] -> [Char] -> ([Char], [Char])) -> Stacks
executeMove stacks (Move n from to) moveFun = IntMap.insert to toStack' (IntMap.insert from fromStack' stacks)
  where
    fromStack = stacks ! from
    toStack = stacks ! to
    (fromStack', toStack') = moveFun n fromStack toStack

topCrates :: Stacks -> String
topCrates stacks = [head (stacks ! n) | n <- [1..(IntMap.size stacks)]]
