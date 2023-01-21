{-# LANGUAGE TupleSections #-}
module Day24 (solve) where
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromJust)
import           Debug.Trace         (trace)
import           Graphs              (BFSOptions (..), shortestDistanceBFSCond)
import           Solution            (Solution (I))
import           StringUtils         (stringsToCharMap)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    groundMap = parseMap input
    (maxX, maxY) = parseDimensions input
    (startNode, endNode) = parseStartEnd maxX maxY
    part1 = solvePart1 groundMap maxX maxY startNode endNode
    part2 = solvePart2 groundMap maxX maxY startNode endNode

type Pos = (Int, Int)

data Dir = DRight | DDown | DLeft | DUp
  deriving Show

type Blizz = Dir

type BlizzMap = HashMap Pos [Blizz]

parseMap :: String -> BlizzMap
parseMap input = HM.map charToGround $ HM.filter (\c -> c /= '#' && c /= '.') charMap
  where
    (charMap, _, _) = stringsToCharMap $ lines input
    charToGround c = case c of
      '>' -> [DRight]
      'v' -> [DDown]
      '<' -> [DLeft]
      '^' -> [DUp]
      _   -> undefined

parseDimensions :: String -> (Int, Int)
parseDimensions input = (width - 1, height - 1)
  where
    width = length $ head $ lines input
    height = length $ lines input

parseStartEnd :: Int -> Int -> (Pos, Pos)
parseStartEnd maxX maxY = (startNode, endNode)
  where
    startNode = (1, 0)
    endNode = (maxX - 1, maxY)

-- Part 1

type Node = (Pos, Int)

solvePart1 :: BlizzMap -> Int -> Int -> Pos -> Pos -> Integer
solvePart1 groundMap maxX maxY startPos endPos = fromJust $ shortestDistanceBFSCond startNode endCond' adjacency options
  where
    startNode = (startPos, 0)
    endCond' = endCond endPos
    options = BFSOptions {pruneFun = Nothing, keepVisited = True}
    adjacency = getAdjacency groundMap maxX maxY startPos endPos

endCond :: Pos -> (Pos, Int) -> Bool
endCond endPos (pos, _) = pos == endPos

-- Part 2

solvePart2 :: BlizzMap -> Int -> Int -> Pos -> Pos -> Integer
solvePart2 groundMap maxX maxY startPos endPos = trace (show (firstTime, returnTime, secondTime)) firstTime + returnTime + secondTime
  where
    firstTime = fromJust $ shortestDistanceBFSCond startNode1 endCond1 adjacency options
    returnTime = fromJust $ shortestDistanceBFSCond startNode2 endCond2 adjacency options
    secondTime = fromJust $ shortestDistanceBFSCond startNode3 endCond3 adjacency options
    startNode1 = (startPos, 0)
    startNode2 = (endPos, fromInteger $ firstTime `mod` toInteger modN)
    startNode3 = (startPos, fromInteger $ (firstTime + returnTime) `mod` toInteger modN)
    endCond1 = endCond endPos
    endCond2 = endCond startPos
    endCond3 = endCond1
    options = BFSOptions {pruneFun = Nothing, keepVisited = True}
    adjacency = getAdjacency groundMap maxX maxY startPos endPos
    modN = lcm (maxX - 1) (maxY - 1)

-- Graph building

getAdjacency :: BlizzMap -> Int -> Int -> Pos -> Pos -> Node -> [Node]
getAdjacency groundMap maxX maxY startPos endPos = getAdjecency' groundMaps maxX maxY startPos endPos modN
  where
    modN = lcm (maxX - 1) (maxY - 1)
    groundMaps = take modN $ iterate (moveBlizzs maxX maxY) groundMap

getAdjecency' :: [BlizzMap] -> Int -> Int -> Pos -> Pos -> Int -> Node -> [Node]
getAdjecency' groundMaps maxX maxY startPos endPos modN (pos@(x, y), n) = map (, n') emptyGround
  where
    directions = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
    insideMap = filter (isInside maxX maxY startPos endPos) directions
    emptyGround = filter (not . flip HM.member (groundMaps !! n')) (pos : insideMap)
    n' = (n + 1) `mod` modN

isInside :: Int -> Int -> Pos -> Pos -> Pos -> Bool
isInside _ _ startPos endPos pos | pos == startPos || pos == endPos = True
isInside maxX maxY _ _ (x, y) = x > 0 && x < maxX && y > 0 && y < maxY

-- Blizzard movement

moveBlizzs :: Int -> Int -> BlizzMap -> BlizzMap
moveBlizzs maxX maxY blizzMap = mergeBlizzMap moved
  where
    moved = concatMap (moveBlizzs' maxX maxY) $ HM.toList blizzMap

moveBlizzs' :: Int -> Int -> (Pos, [Blizz]) -> [(Pos, Blizz)]
moveBlizzs' maxX maxY (pos, blizzs) = map (moveBlizz maxX maxY pos) blizzs

moveBlizz :: Int -> Int -> Pos -> Blizz -> (Pos, Blizz)
moveBlizz maxX maxY (x, y) blizz = (pos'', blizz)
  where
    pos' = case blizz of
      DRight -> (x + 1, y)
      DDown  -> (x, y + 1)
      DLeft  -> (x - 1, y)
      DUp    -> (x, y - 1)
    pos'' = handleWrapping maxX maxY pos'

handleWrapping :: Int -> Int -> Pos -> Pos
handleWrapping maxX maxY pos@(x, y)
  | x < 1 = (maxX - 1, y)
  | x >= maxX = (1, y)
  | y < 1 = (x, maxY - 1)
  | y >= maxY = (x, 1)
  | otherwise = pos

mergeBlizzMap :: [(Pos, Blizz)] -> BlizzMap
mergeBlizzMap = foldr mergeBlizz HM.empty

mergeBlizz :: (Pos, Blizz) -> BlizzMap -> BlizzMap
mergeBlizz (pos, blizz) blizzMap = case HM.lookup pos blizzMap of
  Nothing     -> HM.insert pos [blizz] blizzMap
  Just blizzs -> HM.insert pos (blizz : blizzs) blizzMap
