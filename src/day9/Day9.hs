module Day9 (solve) where
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Solution     (Solution (I))

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    instrs = parseInstrs input
    part1 = toInteger $ HS.size $ tailVisited $ executeInstrs 2 instrs
    part2 = toInteger $ HS.size $ tailVisited $ executeInstrs 10 instrs

data Direction = DUp | DRight | DDown | DLeft
  deriving Show

type Instruction = (Direction, Integer)

parseInstrs :: String -> [Instruction]
parseInstrs input = map parseInstr (lines input)

parseInstr :: String -> Instruction
parseInstr line = (dir, distance)
  where
    tokens = words line
    dir = case head tokens of
      "U" -> DUp
      "R" -> DRight
      "D" -> DDown
      "L" -> DLeft
      _   -> error "Parse error"
    distance = read $ tokens !! 1

type Pos = (Integer, Integer)

type Rope = [Pos]

data RopeState = RopeState{
  rope        :: Rope,
  tailVisited :: HashSet Pos
} deriving Show

startState :: Int -> RopeState
startState n = RopeState {rope = replicate n (0, 0), tailVisited = HS.singleton (0, 0)}

executeInstrs :: Int -> [Instruction] -> RopeState
executeInstrs n = foldl executeInstr (startState n)

executeInstr :: RopeState -> Instruction -> RopeState
executeInstr rs (_, 0) = rs
executeInstr rs (dir, n) = executeInstr rs' (dir, n - 1)
  where
    rs' = moveTail $ moveHead rs dir

moveHead :: RopeState -> Direction -> RopeState
moveHead rs dir = rs{rope = newHead : tail (rope rs)}
  where
    (x, y) = head $ rope rs
    newHead = case dir of
      DUp    -> (x, y + 1)
      DRight -> (x + 1, y)
      DDown  -> (x, y - 1)
      DLeft  -> (x - 1, y)

moveTail :: RopeState -> RopeState
moveTail rs = rs{rope = newRope, tailVisited = HS.insert (last newRope) (tailVisited rs)}
  where
    newRope = moveTail' $ rope rs

moveTail' :: Rope -> Rope
moveTail' (next:current:rope') = next : moveTail' (newCurrent : rope')
  where
    newCurrent = moveTailSegment next current
moveTail' [last'] = [last']
moveTail' [] = error "Empty rope passed to moveTail"

moveTailSegment ::  Pos -> Pos -> Pos
moveTailSegment next current = if adjacent next current
  then current
  else moveTailSegment' next current

moveTailSegment' :: Pos -> Pos -> Pos
moveTailSegment' (xNext, yNext) (xCurrent, yCurrent) = (xCurrent + comp dx, yCurrent + comp dy)
  where
    dx = compare xNext xCurrent
    dy = compare yNext yCurrent
    comp :: Ordering -> Integer
    comp d = case d of
      GT -> 1
      EQ -> 0
      LT -> -1

adjacent :: Pos -> Pos -> Bool
adjacent (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1
