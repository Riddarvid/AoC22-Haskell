module Day21 (solve) where
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Solution (Solution (I))

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    monkeyMap = parseMonkeys input
    root = parseMonkey monkeyMap $ monkeyMap ! "root"
    part1 = solvePart1 root
    part2 = solvePart2 root

data Op = Add | Sub | Mul | Div

data MDExpr = MDNum Integer | MDOp Op String String
data MonkeyData = MD String MDExpr
type MonkeyMap = Map String MonkeyData

data MExpr = MNum Integer | MOp Op Monkey Monkey
data Monkey = Monkey String MExpr

data Expr = ENum Integer | EOp Op Expr Expr

-- Parsing

parseMonkeys :: String -> MonkeyMap
parseMonkeys input = Map.fromList $ map (\str -> let mnk@(MD name _) = parseMD str in (name, mnk)) $ lines input

parseMD :: String -> MonkeyData
parseMD monkeyStr
  | length tokens == 2 = MD name (MDNum n)
  | otherwise = MD name (MDOp op op1 op2)
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

parseMonkey :: MonkeyMap -> MonkeyData -> Monkey
parseMonkey _ (MD name (MDNum n)) = Monkey name $ MNum n
parseMonkey monkeyMap (MD name (MDOp op name1 name2)) = Monkey name $ MOp op exp1 exp2
  where
    monkey1 = monkeyMap ! name1
    monkey2 = monkeyMap ! name2
    exp1 = parseMonkey monkeyMap monkey1
    exp2 = parseMonkey monkeyMap monkey2

-- Utils

pathsToMonkey :: Monkey -> String -> [[String]]
pathsToMonkey (Monkey name expr) goalName
  | name == goalName = [[name]]
  | otherwise = case expr of
    MNum _ -> []
    MOp _ monkey1 monkey2 -> paths1 ++ paths2
      where
        paths1 = map (name :) $ pathsToMonkey monkey1 goalName
        paths2 = map (name :) $ pathsToMonkey monkey2 goalName

monkeyToExpr :: Monkey -> Expr
monkeyToExpr (Monkey _ (MNum n)) = ENum n
monkeyToExpr (Monkey _ (MOp op monkey1 monkey2)) = EOp op (monkeyToExpr monkey1) (monkeyToExpr monkey2)

eval :: Expr -> Integer
eval (ENum n) = n
eval (EOp op exp1 exp2) = case op of
  Add -> val1 + val2
  Sub -> val1 - val2
  Mul -> val1 * val2
  Div -> val1 `div` val2
  where
    val1 = eval exp1
    val2 = eval exp2

-- Part 1

solvePart1 :: Monkey -> Integer
solvePart1 root = eval rootExp
  where
    rootExp = monkeyToExpr root

-- Part 2

solvePart2 :: Monkey -> Integer
solvePart2 root = eval expr
  where
    path = head $ pathsToMonkey root "humn"
    expr = reduceExp root path

reduceExp :: Monkey -> [String] -> Expr
reduceExp = reduce (ENum 0)

reduce :: Expr -> Monkey -> [String] -> Expr
reduce expr (Monkey "humn" _) _ = expr
reduce expr (Monkey name expr') path
  | name == "root" = reduce val nextMonkey path'
  | otherwise = reduce expr'' nextMonkey path'
  where
    (op, monkey1@(Monkey name1 _), monkey2) = case expr' of
      (MOp op' name1' name2') -> (op', name1', name2')
      _                       -> undefined
    firstInPath = name1 `elem` path
    val = ENum $ if firstInPath
      then eval $ monkeyToExpr monkey2
      else eval $ monkeyToExpr monkey1
    expr'' = if firstInPath
      then case op of
        Add -> EOp Sub expr val
        Sub -> EOp Add expr val
        Mul -> EOp Div expr val
        Div -> EOp Mul expr val
      else case op of
        Add -> EOp Sub expr val
        Sub -> EOp Sub val expr
        Mul -> EOp Div expr val
        Div -> EOp Div val expr
    path' = tail path
    nextMonkey = if firstInPath then monkey1 else monkey2
