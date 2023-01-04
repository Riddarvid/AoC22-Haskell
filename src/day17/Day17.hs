{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Day17 (solve) where
import           Control.Monad.State (MonadState (get, put), State, evalState,
                                      execState, gets, modify, unless, when)
import           Data.Maybe          (fromJust, isJust, isNothing)
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Debug.Trace         (trace)
import           Solution            (Solution (I, S))

solve :: String -> (Solution, Solution)
solve input = (S $ showState part1, I 0)
  where
    jets = parseInput input
    part1 = solvePart1 jets
    --(_, height1) = stackRocks Set.empty 0 cycledJets $ take 2022 baseRocks
    --(_, height2) = stackRocks Set.empty 0 cycledJets $ take 1000000000000 baseRocks
    --cycleLength = length jets
    --(_, cycleHeight) = stackRocks Set.empty 0 cycledJets $ take (2 * cycleLength) baseRocks
    --nCycles = 1000000000000 `div` toInteger cycleLength
    --offset = 1000000000000 `mod` cycleLength
    --(_, offsetHeight) = stackRocks cycledJets $ take offset baseRocks
    --height2 = nCycles * cycleHeight + offsetHeight

data Direction = DLeft | DRight | DDown
  deriving Show

newtype Jet = Jet Direction
  deriving Show

-- Parsing

parseInput :: String -> [Jet]
parseInput input = map parseJet $ head $ lines input

parseJet :: Char -> Jet
parseJet '<' = Jet DLeft
parseJet '>' = Jet DRight
parseJet c   = error $ "Illegal character: " ++ show c

-- General

type Pos = (Integer, Integer)

newtype Rock = Rock (Set Pos)
  deriving Show

baseRocks :: [Rock]
baseRocks = cycle [
  Rock $ Set.fromList [(0, 0), (1, 0), (2, 0), (3, 0)],
  Rock $ Set.fromList [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
  Rock $ Set.fromList [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
  Rock $ Set.fromList [(0, 0), (0, 1), (0, 2), (0, 3)],
  Rock $ Set.fromList [(0, 0), (1, 0), (0, 1), (1, 1)]]

-- Rocks state

data RocksState = RocksState{
  rPlaced  :: Set Pos,
  rRocks   :: [Rock],
  rJets    :: [Jet],
  rHeight  :: Integer,
  rCurrent :: Maybe Rock,
  rFallen  :: Integer
} deriving Show

newState :: [Rock] -> [Jet] -> RocksState
newState rocks jets = RocksState {rPlaced = Set.empty, rRocks = rocks, rJets = jets, rHeight = 0, rCurrent = Nothing, rFallen = 0}

takeRock :: State RocksState Rock
takeRock = do
  rock <- gets (head . rRocks)
  modify (\s -> s{rRocks = tail $ rRocks s})
  return rock

takeJet :: State RocksState Jet
takeJet = do
  jet <- gets (head . rJets)
  modify (\s -> s{rJets = tail $ rJets s})
  return jet

getCurrent :: State RocksState Rock
getCurrent = do
  rocksState <- get
  case rCurrent rocksState of
    Just current -> return current
    Nothing -> do
      height <- gets rHeight
      nextRock <- takeRock
      let nextRock' = setRockSpawnHeight nextRock height
      modify (\s ->s{rCurrent = Just nextRock', rFallen = rFallen s + 1})
      return nextRock'

isFalling :: State RocksState Bool
isFalling = gets (isJust . rCurrent)

-- Functions for manipulating rock state. The basic operation is letting one time unit (tick) pass.

stackRocks :: Integer -> State RocksState ()
stackRocks 0 = return ()
stackRocks n = do
  stackRock
  stackRocks (n - 1)

stackRock :: State RocksState ()
stackRock = do
  tick
  falling <- isFalling
  when falling stackRock

tick :: State RocksState ()
tick = do
  shiftCurrent
  fallCurrent
  --placed <- gets rPlaced
  --current <- gets rCurrent
  --height <- gets rHeight
  --trace (showTower placed current (height + 10)) (return ())

shiftCurrent :: State RocksState ()
shiftCurrent = do
  (Jet direction) <- takeJet
  placed <- gets rPlaced
  current <- getCurrent
  let newRock = if canMove placed current direction then moveRock current direction else current
  modify (\s -> s{rCurrent = Just newRock})

fallCurrent :: State RocksState ()
fallCurrent = do
  placed <- gets rPlaced
  current <- getCurrent
  if canMove placed current DDown then modify (\s -> s{rCurrent = Just $ moveRock current DDown}) else stopCurrent

stopCurrent :: State RocksState ()
stopCurrent = do
  rock@(Rock rockPoss) <- gets (fromJust . rCurrent)
  modify (\s -> s{rCurrent = Nothing, rPlaced = Set.union rockPoss (rPlaced s), rHeight = max (rockHeight rock) (rHeight s)})

-- Utils

rockHeight :: Rock -> Integer
rockHeight (Rock rockPoints) = maximum $ Set.map snd rockPoints -- Max y coord

canMove :: Set Pos -> Rock -> Direction -> Bool
canMove placed rock direction = not $ any (\pos@(x, y) -> Set.member pos placed || x < 0 || x > 6 || y <= 0) rockPoints'
  where
    (Rock rockPoints') = moveRock rock direction

moveRock :: Rock -> Direction -> Rock
moveRock (Rock rockPoints) direction = Rock $ Set.map moveFun rockPoints
  where
    moveFun = case direction of
      DLeft  -> (\(x, y) -> (x - 1, y))
      DRight -> (\(x, y) -> (x + 1, y))
      DDown  -> (\(x, y) -> (x, y - 1))

setRockSpawnHeight :: Rock -> Integer -> Rock
setRockSpawnHeight (Rock poss) height = Rock $ Set.map (\(x, y) -> (x + 2, height + 4 + y)) poss

showState :: RocksState -> String
showState s =
  "\n\nNumber of spawned blocks: " ++ show (rFallen s) ++
  "\n\nCurrent height: " ++ show height ++
  "\n\nCurrently falling: " ++ show current ++
  "\n\nNext few rocks to fall: " ++ show (take 5 (rRocks s)) ++
  "\n\nNext few jets: " ++ show (take 5 (rJets s)) ++
  --"\n\nPlaced: " ++ show (rPlaced s) ++
  "\n\nHighest ten levels:\n\n" ++ showTower' upperLevels current (height - 10) (height + 2)
  where
    height = rHeight s
    current = rCurrent s
    upperLevels = Set.filter (\(_, y) -> y >= height - 10) (rPlaced s)

showTower :: Set Pos -> Maybe Rock -> Integer -> String
showTower placed current = showTower' placed current 1

showTower' :: Set Pos -> Maybe Rock -> Integer -> Integer -> String
showTower' placed current bottom top = unlines $ map (showLayer placed current) [top, top - 1 .. bottom]

showLayer :: Set Pos -> Maybe Rock -> Integer -> String
showLayer placed current layer = [showPos placed current (x, layer) | x <- [0 .. 6]]

showPos :: Set Pos -> Maybe Rock -> Pos -> Char
showPos placed current pos
  | Set.member pos placed = '#'
  | otherwise = case current of
    Just (Rock poss) -> if Set.member pos poss then '@' else '.'
    Nothing          -> '.'

rPositions :: Maybe Rock -> Set Pos
rPositions rock = case rock of
  Just (Rock poss) -> poss
  Nothing          -> Set.empty

-- Part 1

solvePart1 :: [Jet] -> RocksState
solvePart1 jets = state
  where
    state = execState (stackRocks 2022) startState
    startState = newState rocks jets'
    jets' = cycle jets
    rocks = cycle baseRocks
