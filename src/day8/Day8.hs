module Day8 (solve) where
import           Data.Char         (digitToInt)
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM
import           Solution          (Solution (I))

type Pos = (Int, Int)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    maxPos = (length (head (lines input)) - 1, length (lines input) - 1)
    treeMap = parseInput input maxPos
    part1 = countVisible treeMap maxPos
    part2 = maxScenicScore treeMap maxPos

parseInput :: String -> Pos -> HashMap Pos Int
parseInput input (maxX, maxY) = HM.fromList mapList
  where
    heights = map digitToInt $ concat $ lines input
    mapList = zip [(x, y) | y <- [0 .. maxY], x <- [0 .. maxX]] heights

-- Part 1

countVisible :: HashMap Pos Int -> Pos -> Integer
countVisible treeMap maxPos = toInteger $ length $ filter (isVisible treeMap maxPos) (HM.keys treeMap)

isVisible :: HashMap Pos Int -> Pos -> Pos -> Bool
isVisible treeMap maxPos pos = any allLower $ directionHeights treeMap maxPos pos
  where
    height = treeMap ! pos
    allLower = all (< height)

-- Part 2

maxScenicScore :: HashMap Pos Int -> Pos -> Integer
maxScenicScore treeMap maxPos = maximum $ map (scenicScore treeMap maxPos) (HM.keys treeMap)

scenicScore :: HashMap Pos Int -> Pos -> Pos -> Integer
scenicScore treeMap maxPos pos = product distances
  where
    height = treeMap ! pos
    directions = directionHeights treeMap maxPos pos
    distances = map (viewDistance height) directions

viewDistance :: Int -> [Int] -> Integer
viewDistance height neighborHeights
  | null neighborHeights = 1                                                                -- Tree is at edge
  | length visibleNeighbors == length neighborHeights = toInteger $ length visibleNeighbors -- View is not blocked
  | otherwise = toInteger $ length visibleNeighbors + 1                                     -- View is blocked
  where
    visibleNeighbors = takeWhile (< height) neighborHeights

-- General

directionHeights :: HashMap Pos Int -> Pos -> Pos -> [[Int]]
directionHeights treeMap (maxX, maxY) (x, y) = [northHeights, eastHeights, southHeights, westHeights]
  where
    northHeights = [treeMap ! (x, ny) | ny <- [y + 1 .. maxY]]
    eastHeights = [treeMap ! (nx, y) | nx <- [x + 1 .. maxX]]
    southHeights = [treeMap ! (x, ny) | ny <- [y - 1, y - 2 .. 0]]
    westHeights = [treeMap ! (nx, y) | nx <- [x - 1, x - 2 .. 0]]
