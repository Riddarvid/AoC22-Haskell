module Day12 (solve) where
import           Data.Char         (ord)
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe        (fromJust)
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Graphs            (Path, nodesFromPath, shortestPathBFS,
                                    shortestPathBFS')
import           Solution          (Solution (S))
import           StringUtils       (stringsToCharMap)

solve :: String -> (Solution, Solution)
solve input = (S (showPath charMap path1 maxX maxY ++ "\n\n" ++ show part1), S (showPath charMap path2 maxX maxY ++ "\n\n" ++ show part2))
  where
    (charMap, maxX, maxY) = stringsToCharMap $ lines input
    startPos = findPosition 'S' charMap
    goalPos = findPosition 'E' charMap
    adjacencyFun node = adjacencyTable charMap ! node
    path1 = fromJust $ shortestPathBFS startPos goalPos adjacencyFun Nothing
    part1 = toInteger $ length path1
    startPos' = findPositions 'a' charMap
    path2 = fromJust $ shortestPathBFS' startPos' goalPos adjacencyFun Nothing
    part2 = toInteger $ length path2

type Pos = (Int, Int)

findPosition :: Char -> HashMap Pos Char -> Pos
findPosition target charMap = head $ findPositions target charMap

findPositions :: Char -> HashMap Pos Char -> [Pos]
findPositions target charMap = map fst $ filter (\(_, val) -> val == target) (HM.toList charMap)

adjacencyTable :: HashMap Pos Char -> HashMap Pos [Pos]
adjacencyTable charMap = HM.mapWithKey (adjacentNeighbors charMap) charMap

adjacentNeighbors :: HashMap Pos Char -> Pos -> Char -> [Pos]
adjacentNeighbors charMap currPos currChar = filter (\pos -> reachableFrom currChar (charMap ! pos)) (neighborsInMap currPos charMap)

reachableFrom :: Char -> Char -> Bool
reachableFrom 'S' to   = reachableFrom 'a' to
reachableFrom from 'S' = reachableFrom from 'a'
reachableFrom 'E' to   = reachableFrom 'z' to
reachableFrom from 'E' = reachableFrom from 'z'
reachableFrom from to  = ord to <= ord from + 1

neighborsInMap :: Pos -> HashMap Pos a -> [Pos]
neighborsInMap pos adjacencies = filter (`HM.member` adjacencies) (neighbors pos)

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

-- Print path

red :: String
red = "\ESC[31m"

reset :: String
reset = "\ESC[0m"

showRed :: Char -> String
showRed c = red ++ [c] ++ reset

showPath :: HashMap Pos Char -> Path Pos -> Int -> Int -> String
showPath charMap path maxX maxY = unlines $ [showLine charMap (nodesFromPath path) maxX y | y <- [0 .. maxY]]

showLine :: HashMap Pos Char -> Set Pos -> Int -> Int -> String
showLine charMap path maxX y = foldr (\pos str -> showPos charMap path pos ++ str) "" ([(x, y) | x <- [0 .. maxX]])

showPos :: HashMap Pos Char -> Set Pos -> Pos -> String
showPos charMap path pos
  | pos `Set.member` path = showRed c
  | otherwise = [c]
  where
    c = charMap ! pos
