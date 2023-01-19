module Day21 (solve) where
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Solution (Solution (I, S))

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    monkeyMap = parseMonkeys input
    root = monkeyMap ! "root"
    part1 = eval monkeyMap root
    part2 = solvePart2 monkeyMap

data Op = Add | Sub | Mul | Div

data Expr = MNum Integer | MOp Op String String

data Monkey = Monkey String Expr

type MonkeyMap = Map String Monkey

-- Parsing

parseMonkeys :: String -> MonkeyMap
parseMonkeys input = Map.fromList $ map (\str -> let mnk@(Monkey name _) = parseMonkey str in (name, mnk)) $ lines input

parseMonkey :: String -> Monkey
parseMonkey monkeyStr
  | length tokens == 2 = Monkey name (MNum n)
  | otherwise = Monkey name (MOp op op1 op2)
  where
    tokens = words monkeyStr
    name = init $ head tokens
    n = read $ tokens !! 1
    op1 = tokens !! 1
    op2 = tokens !! 3
    op = case tokens !! 2 of
      "+" -> Add
      "-" -> Sub
      "*" -> Mul
      "/" -> Div
      _   -> undefined

-- Eval monkey

eval :: MonkeyMap -> Monkey -> Integer
eval _ (Monkey _ (MNum n)) = n
eval monkeyMap (Monkey _ (MOp op name1 name2)) = case op of
  Add -> val1 + val2
  Sub -> val1 - val2
  Mul -> val1 * val2
  Div -> val1 `div` val2
  where
    val1 = eval monkeyMap (monkeyMap ! name1)
    val2 = eval monkeyMap (monkeyMap ! name2)

-- Utils

pathsToMonkey :: MonkeyMap -> Monkey -> String -> [[String]]
pathsToMonkey monkeyMap (Monkey name expr) goalName
  | name == goalName = [[name]]
  | otherwise = case expr of
    MNum _ -> []
    MOp _ name1 name2 -> paths1 ++ paths2
      where
        paths1 = map (name :) $ pathsToMonkey monkeyMap (monkeyMap ! name1) goalName
        paths2 = map (name :) $ pathsToMonkey monkeyMap (monkeyMap ! name2) goalName

-- Part 1


-- Part 2

data Expr' = ENum Integer | EOp Op Expr' Expr'

eval' :: Expr' -> Integer
eval' (ENum n) = n
eval' (EOp op exp1 exp2) = case op of
  Add -> val1 + val2
  Sub -> val1 - val2
  Mul -> val1 * val2
  Div -> val1 `div` val2
  where
    val1 = eval' exp1
    val2 = eval' exp2

solvePart2 :: MonkeyMap -> Integer
solvePart2 monkeyMap = eval' expr
  where
    root = monkeyMap ! "root"
    path = head $ pathsToMonkey monkeyMap root "humn"
    expr = reduceExp monkeyMap path

reduceExp :: MonkeyMap -> [String] -> Expr'
reduceExp monkeyMap path = foldl (reduceStep monkeyMap path) (ENum 0) path

reduceStep :: MonkeyMap -> [String] -> Expr' -> String -> Expr'
reduceStep = undefined

-- Cases to consider:
-- Name is root
-- Name is humn
-- All other cases
