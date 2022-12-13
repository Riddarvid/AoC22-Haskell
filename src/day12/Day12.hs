module Day12 (solve) where
import           Data.Char         (ord)
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM
import           Data.Maybe        (fromJust)
import           Graphs            (shortestPathBFS, shortestPathBFS')
import           Solution          (Solution (I))
import           StringUtils       (stringsToCharMap)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    (charMap, maxX, maxY) = stringsToCharMap $ lines input
    startPos = findPosition 'S' charMap
    goalPos = findPosition 'E' charMap
    adjacencyFun node = adjacencyTable charMap ! node
    part1 = toInteger $ length $ fromJust $ shortestPathBFS startPos goalPos adjacencyFun
    startPos' = findPositions 'a' charMap
    part2 = toInteger $ length $ fromJust $ shortestPathBFS' startPos' goalPos adjacencyFun

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

