module Day1 (solve) where
import           Data.List       (sort)
import           Data.List.Utils (split)

solve :: String -> (Integer, Integer)
solve input = (part1, part2)
  where
    invs = getInventories input
    part1 = maxCalories 1 invs
    part2 = maxCalories 3 invs

-- Solution

maxCalories :: Int -> [Inventory] -> Integer
maxCalories n xs = sum $ take n $ reverse $ sort $ map sum xs

-- Parsing

type Inventory = [Integer]

getInventories :: String -> [Inventory]
getInventories input = map (map read) (split [""] (lines input))
