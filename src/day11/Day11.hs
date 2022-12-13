{-# LANGUAGE InstanceSigs #-}
module Day11 (solve) where
import           Control.Monad.State (State, execState, get, put)
import           Data.IntMap         (IntMap, (!))
import qualified Data.IntMap         as IM
import           Data.List           (sort)
import           Data.List.Utils     (split)
import           Solution            (Solution (I))
import           StringUtils         (getInts)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    monkeys = parseInput input
    reduceFun1 n = n `div` 3
    mb1 = monkeyBusiness $ execState (executeRounds reduceFun1 20) (newMonkeyState monkeys)
    part1 = product $ take 2 $ reverse $ sort $ IM.elems mb1
    reduceFun2 n = n `mod` smallestFactor monkeys
    mb2 = monkeyBusiness $ execState (executeRounds reduceFun2 10000) (newMonkeyState monkeys)
    part2 = product $ take 2 $ reverse $ sort $ IM.elems mb2

smallestFactor :: IntMap Monkey -> Integer
smallestFactor monkeys = product $ map testDivisor (IM.elems monkeys)


type ReduceFun = (Integer -> Integer)

data Monkey = Monkey{
  monkeyId    :: Int,
  items       :: [Integer],
  operation   :: Integer -> Integer,
  testDivisor :: Integer,
  test        :: Integer -> Bool,
  trueMonkey  :: Int,
  falseMonkey :: Int
}

instance Show Monkey where
  show :: Monkey -> String
  show monkey = "\nMonkey id: " ++ show (monkeyId monkey) ++ "\nItems: " ++ show (items monkey) ++ "\nIf true to: " ++ show (trueMonkey monkey) ++ "\nIf false to:" ++ show (falseMonkey monkey) ++ "\n"

-- Parsing

parseInput :: String -> IntMap Monkey
parseInput input = IM.fromDistinctAscList $ zip [0 ..] $ map parseMonkey $ split [""] (lines input)

parseMonkey :: [String] -> Monkey
--parseMonkey input | trace (show input) False = undefined
parseMonkey [idStr, itemsStr, opStr, testStr, trueStr, falseStr] = Monkey {
  monkeyId = fromInteger $ head $ getInts idStr,
  items = reverse $ getInts itemsStr,
  operation = operation',
  testDivisor = head (getInts testStr),
  test = test',
  trueMonkey = fromInteger $ head $ getInts trueStr,
  falseMonkey = fromInteger $ head $ getInts falseStr}
  where
    operation'
      | words opStr !! 5 == "old" = \x -> x `op` x
      | otherwise = (`op` c)
      where
        op = case words opStr !! 4 of
          "+" -> (+)
          "*" -> (*)
          _   -> error "Parse error"
        c = head (getInts opStr)
    test' = \x -> x `mod` n == 0 where n = head (getInts testStr)
parseMonkey _ = error "Parse error"

-- Part 1

data MonkeyState = MonkeyState{
  monkeyMap      :: IntMap Monkey,
  monkeyBusiness :: IntMap Integer
} deriving Show

newMonkeyState :: IntMap Monkey -> MonkeyState
newMonkeyState monkeys = MonkeyState {monkeyMap = monkeys, monkeyBusiness = IM.fromList [(mId, 0) | mId <- IM.keys monkeys]}

executeRounds :: ReduceFun -> Integer -> State MonkeyState MonkeyState
executeRounds _ 0 = get
executeRounds reduceFun n = do
  executeRound reduceFun
  executeRounds reduceFun (n - 1)

updateMonkeyBusiness :: Monkey -> State MonkeyState ()
updateMonkeyBusiness target = do
  ms <- get
  let oldMb = monkeyBusiness ms ! monkeyId target
  let mbInc = toInteger $ length $ items target
  put ms{monkeyBusiness = IM.insert (monkeyId target) (oldMb + mbInc) (monkeyBusiness ms)}

executeRound :: ReduceFun -> State MonkeyState ()
executeRound reduceFun = do
  ms <- get
  mapM_ (executeTurn reduceFun) [0 .. IM.size (monkeyMap ms) - 1]

executeTurn :: ReduceFun -> Int -> State MonkeyState ()
executeTurn reduceFun currentId = do
  ms <- get
  let current = monkeyMap ms ! currentId
  updateMonkeyBusiness current
  mapM_ (throwItem reduceFun current) (reverse $ items current)
  removeItems currentId

throwItem :: ReduceFun -> Monkey -> Integer -> State MonkeyState ()
throwItem reduceFun current item = do
  let item' = reduceFun $ operation current item
  let target = if test current item' then trueMonkey current else falseMonkey current
  addItem target item'

addItem :: Int -> Integer -> State MonkeyState ()
addItem target item = do
  ms <- get
  let targetMonkey = monkeyMap ms ! target
  let targetMonkey' = targetMonkey{items = item : items targetMonkey}
  put ms{monkeyMap = IM.insert target targetMonkey' (monkeyMap ms)}

removeItems :: Int -> State MonkeyState ()
removeItems target = do
  ms <- get
  let targetMonkey = monkeyMap ms ! target
  let targetMonkey' = targetMonkey{items = []}
  put ms{monkeyMap = IM.insert target targetMonkey' (monkeyMap ms)}
