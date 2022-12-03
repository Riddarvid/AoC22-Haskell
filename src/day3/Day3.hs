module Day3 (solve) where
import qualified Data.HashSet   as HS
import           Data.Maybe     (fromJust)
import           GHC.Utils.Misc (chunkList)

solve :: String -> (Integer, Integer)
solve input = (part1, part2)
  where
    rucksacks = lines input
    part1 = errPrio rucksacks
    part2 = badgePrio rucksacks

-- Part 1

errPrio :: [String] -> Integer
errPrio rucksacks = totalPrio $ map compartments rucksacks

compartments :: String -> [String]
compartments str = [a, b]
  where
    (a, b) = splitAt (length str `div` 2) str

-- Part 2

badgePrio :: [String] -> Integer
badgePrio rucksacks = totalPrio $ chunkList 3 rucksacks

-- General

totalPrio :: [[String]] -> Integer
totalPrio groups = sum $ map (charPrio . findCommon) groups

charPrio :: Char -> Integer
charPrio err = fromJust $ lookup err $ zip (['a'..'z'] ++ ['A'..'Z']) [1..]

findCommon :: [String] -> Char
findCommon [] = error "No strings provided"
findCommon strs = head $ HS.toList $ foldr HS.intersection (head sets) sets
  where
    sets = map HS.fromList strs
