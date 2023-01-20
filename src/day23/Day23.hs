module Day23 (solve) where
import           Control.DeepSeq     (($!!))
import           Data.Bifunctor      (bimap)
import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Data.List           (find, nub)
import           Data.List.Utils     (countElem)
import           Debug.Trace         (trace)
import           Solution            (Solution (I))
import           StringUtils         (showGrid, stringsToCharMap)
import           Utils               (showMap)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    elfSet = parseInput input
    part1 = solvePart1 elfSet
    part2 = solvePart2 elfSet

type Pos = (Int, Int)

-- A set representing all the locations where elves are
type ElfSet = HashSet Pos

parseInput :: String -> ElfSet
parseInput input = HM.keysSet $ HM.filter (== '#') charMap
  where
    (charMap, _, _) = stringsToCharMap $ lines input

-- Part 1

solvePart1 :: ElfSet -> Integer
solvePart1 elfSet = score endSet
  where
    endSet = snd $ iterate (uncurry doRound) (0, elfSet) !! 10

-- Part 2

solvePart2 :: ElfSet -> Integer
solvePart2 elfSet = toInteger $ noMoveTurn iterations
  where
    iterations = iterate (uncurry doRound $!!) (0, elfSet)

noMoveTurn :: [(Int, ElfSet)] -> Int
noMoveTurn (a:b:xs)
  | snd a == snd b = fst b
  | otherwise = noMoveTurn (b:xs)
noMoveTurn _ = undefined

-- General

-- Round logic

doRound :: Int -> ElfSet -> (Int, ElfSet)
doRound turn elfSet = (turn + 1, elfSet')
  where
    moves = getMoves turn elfSet
    targets = map snd $ HS.toList moves
    uniqueTargets = keepUnique targets
    uniqueMoves = HM.fromList $ HS.toList $ HS.filter (\(_, target) -> HS.member target uniqueTargets) moves
    elfSet' = HS.map (moveFun uniqueMoves) elfSet

keepUnique :: [Pos] -> HashSet Pos
keepUnique = keepUnique' HS.empty HS.empty

keepUnique' :: HashSet Pos -> HashSet Pos -> [Pos] -> HashSet Pos
keepUnique' unique _ [] = unique
keepUnique' unique notUnique (x : xs)
  | HS.member x notUnique = keepUnique' unique notUnique xs
  | HS.member x unique = keepUnique' (HS.delete x unique) (HS.insert x notUnique) xs
  | otherwise = keepUnique' (HS.insert x unique) notUnique xs

moveFun :: HashMap Pos Pos -> Pos -> Pos
moveFun moveMap pos
  | HM.member pos moveMap = moveMap ! pos
  | otherwise = pos

getMoves :: Int -> ElfSet -> HashSet (Pos, Pos)
getMoves turn elfSet = HS.map (\pos -> (pos, getMove turn elfSet pos)) elfSet

-- First check if alone
-- Then check directions one by one
getMove :: Int -> ElfSet -> Pos -> Pos
getMove turn elfSet pos
  | alone elfSet pos = pos
  | otherwise = case find (allFree elfSet) (turnDirections turn pos) of
    Nothing   -> pos
    Just side -> side !! 1 -- The middle element of each side is the direction to go in

alone :: ElfSet -> Pos -> Bool
alone elfSet pos = all (\pos' -> pos' == pos || not (HS.member pos' elfSet)) [addPos pos (x', y') | x' <- side, y' <- side]
  where
    side = [-1, 0, 1]

allFree :: ElfSet -> [Pos] -> Bool
allFree elfSet = not . any (`HS.member` elfSet)

turnDirections :: Int -> Pos -> [[Pos]]
turnDirections turn pos = map (map (addPos pos)) (end ++ start)
  where
    (start, end) = splitAt (turn `mod` 4) directions

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

directions :: [[Pos]]
directions = [
  [(x, -1) | x <- side], -- North
  [(x, 1)  | x <- side], -- South
  [(-1, y) | y <- side], -- West
  [(1, y)  | y <- side]  -- East
  ]
  where
    side = [-1, 0, 1]

-- Scoring

score :: ElfSet -> Integer
score elfSet = totalTiles - toInteger (HS.size elfSet)
  where
    (minX, maxX, minY, maxY) = getDimensions elfSet
    totalTiles = toInteger (maxX - minX + 1) * toInteger (maxY - minY + 1)

getDimensions :: ElfSet -> (Int, Int, Int, Int)
getDimensions elfSet = HS.foldr updateDimensions (startX, startX, startY, startY) elfSet
  where
    (startX, startY) = head $ HS.toList elfSet

updateDimensions :: Pos -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
updateDimensions (x, y) (minX, maxX, minY, maxY) = (min x minX, max x maxX, min y minY, max y maxY)
