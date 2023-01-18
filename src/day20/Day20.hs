{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day20 (solve, propNoHoles, propSameLength) where
import           Data.Foldable      (Foldable (foldl'), find)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.List          (sort, sortBy)
import           Data.Maybe         (fromJust)
import           Solution           (Solution (I))
import           Test.QuickCheck    (Property, (==>))

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    myList = parseInput input
    part1 = solvePart1 myList
    part2 = solvePart2 myList


-- (val, modVal, index)
type Entry = (Integer, Int, Int)

-- Map from id to entry
type MyList = IntMap Entry

-- Parsing

parseInput :: String -> MyList
parseInput input = toMyList integerList
  where
    integerList = map read $ lines input

toMyList :: [Integer] -> MyList
toMyList integerList = IntMap.fromList $ zip [0 ..] $ zip3 integerList (map (\n -> fromInteger n `mod` (length integerList - 1)) integerList) [0 .. ]

-- Part 1

solvePart1 :: MyList -> Integer
solvePart1 list = (list' ! i1) + (list' ! i2) + (list' ! i3)
  where
    list' = mix list
    zeroIndex = indexOfVal 0 list'
    i1 = zeroIndex + 1000
    i2 = zeroIndex + 2000
    i3 = zeroIndex + 3000

-- Part 2

solvePart2 :: MyList -> Integer
solvePart2 list = (list' ! i1) + (list' ! i2) + (list' ! i3)
  where
    encrypted = encrypt list 811589153
    list' = iterate (mix) encrypted !! 10
    zeroIndex = indexOfVal 0 list'
    i1 = zeroIndex + 1000
    i2 = zeroIndex + 2000
    i3 = zeroIndex + 3000

encrypt :: MyList -> Integer -> MyList
encrypt list key = IntMap.map (\(val, _, i) -> (val * key,  fromInteger ((val * key) `mod` toInteger (IntMap.size list - 1)), i)) list

-- General

mix :: MyList -> MyList
mix list = foldl' mixStep list [0 .. (myLength list - 1)]

mixStep :: MyList -> Int -> MyList
mixStep list myId = IntMap.mapWithKey (updateIndex myId oldIndex newIndex) (IntMap.insert myId (val, modVal, newIndex) list)
  where
    (val, modVal, oldIndex) = list IntMap.! myId
    newIndex = if (oldIndex + modVal) >= IntMap.size list
      then (oldIndex + modVal) - IntMap.size list + 1
      else oldIndex + modVal

updateIndex :: Int -> Int -> Int -> Int -> Entry -> Entry
updateIndex movedId oldIndex newIndex myId (val, modVal, i)
  | myId == movedId = (val, modVal, i)
  | oldIndex < i && i <= newIndex = (val, modVal, i - 1)
  | newIndex <= i && i < oldIndex = (val, modVal, i + 1)
  | otherwise = (val, modVal, i)

-- List utils

indexOfVal :: Integer -> MyList -> Int
indexOfVal val list = fromJust $ lookup val $ map (\(v, _, i) -> (v, i)) $ IntMap.elems list

(!) :: MyList -> Int -> Integer
list ! i = fst $ fromJust $ find (\(_, i') -> i' == modI) $ map (\(v, _, i'') -> (v, i'')) $ IntMap.elems list
  where
    modI = i `mod` IntMap.size list

member :: MyList -> Int -> Bool
member list n = IntMap.member n list

myLength :: MyList -> Int
myLength = IntMap.size

-- Props

propNoHoles :: MyList -> Int -> Property
propNoHoles list myId = list `member` myId && myLength list > 1 ==> noHoles mixed
  where
    mixed = mixStep list myId

noHoles :: MyList -> Bool
noHoles list = sort (map (\(_, _, i) -> i) (IntMap.elems list)) == [0 .. (IntMap.size list - 1)]

propSameLength :: MyList -> Int -> Property
propSameLength list myId = list `member` myId && myLength list > 1 ==> myLength list == myLength mixed
  where
    mixed = mixStep list myId

-- Utils

showList' :: MyList -> String
showList' list = show $ map fst $ sortBy (\a b -> compare (snd a) (snd b)) $ map (\(v, _, i) -> (v, i)) $ IntMap.elems list
