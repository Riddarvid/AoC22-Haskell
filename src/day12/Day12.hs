module Day12 (solve) where
import           Data.Char         (ord)
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM
import           Data.HashSet      (HashSet)
import qualified Data.HashSet      as HS
import           Solution          (Solution (I))
import           StringUtils       (stringsToCharMap)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    charMap = stringsToCharMap $ lines input
    startPos = findPosition 'S' charMap
    goalPos = findPosition 'E' charMap
    adjacency = adjacencyTable charMap
    part1 = breadthFirst (HS.singleton startPos) (HS.singleton startPos) adjacency goalPos
    startPos' = findPositions 'a' charMap
    part2 = breadthFirst (HS.fromList startPos') (HS.fromList startPos') adjacency goalPos

type Pos = (Int, Int)

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

findPosition :: Char -> HashMap Pos Char -> Pos
findPosition target charMap = head $ findPositions target charMap

findPositions :: Char -> HashMap Pos Char -> [Pos]
findPositions target charMap = map fst $ filter (\(_, val) -> val == target) (HM.toList charMap)

breadthFirst :: HashSet Pos -> HashSet Pos -> HashMap Pos [Pos] -> Pos -> Integer
breadthFirst reachedLast visited adjacency goal
  | goal `elem` reachedLast = 0
  | otherwise = if HS.null reachedLast'
    then error "No path to goal exists"
    else 1 + breadthFirst reachedLast' visited' adjacency goal
  where
    reachedLast' = newNodes reachedLast visited adjacency
    visited' = HS.union reachedLast' visited

-- Takes the positions reached last iteration, the visited positions and an adjecency map
-- returns the new positions found
newNodes :: HashSet Pos -> HashSet Pos -> HashMap Pos [Pos] -> HashSet Pos
newNodes reachedLast visited adjacency = foldr (HS.union . newNeighbors visited adjacency) HS.empty reachedLast

newNeighbors :: HashSet Pos -> HashMap Pos [Pos] -> Pos -> HashSet Pos
newNeighbors visited adjacency current = HS.fromList $ filter
  (\neighbor -> not (HS.member neighbor visited) && elem neighbor (adjacency ! current)) (neighborsInMap current adjacency)

neighborsInMap :: Pos -> HashMap Pos a -> [Pos]
neighborsInMap pos adjacencies = filter (`HM.member` adjacencies) (neighbors pos)

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
