module Day10 (solve) where
import           GHC.Utils.Misc (chunkList)
import           Solution       (Solution (I, S))

solve :: String -> (Solution, Solution)
solve input = (I part1, S rendered)
  where
    instrs = parseInstrs input
    xVals = zip [1 ..] (computeX instrs)
    part1 = sum $ map signalStrength $ takeEveryNth 40 (drop 19 xVals)
    rendered = unlines $ chunkList 40 $ map (\(cycleVal, x) -> if abs ((cycleVal - 1) `mod` 40 - x) <= 1 then '#' else '.') xVals

-- Parsing

data Instruction = Noop | Addx Integer
  deriving Show

parseInstrs :: String -> [Instruction]
parseInstrs input = map parseInstr $ lines input

parseInstr :: String -> Instruction
parseInstr line = case head tokens of
  "noop" -> Noop
  "addx" -> Addx $ read $ tokens !! 1
  _      -> error "Parse error"
  where
    tokens = words line

-- Part 1

computeX :: [Instruction] -> [Integer]
computeX instrs = init $ 1 : computeX' instrs 1

computeX' :: [Instruction] -> Integer -> [Integer]
computeX' [] _                 = []
computeX' (Noop : instrs) x    = x : computeX' instrs x
computeX' (Addx dx : instrs) x = x : x + dx : computeX' instrs (x + dx)

takeEveryNth :: Int -> [a] -> [a]
takeEveryNth _ [] = []
takeEveryNth n xs = head xs : takeEveryNth n (drop n xs)

signalStrength :: Num a => (a, a) -> a
signalStrength (cycleVal, x) = cycleVal * x
