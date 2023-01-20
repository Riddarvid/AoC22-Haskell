module Day22 (solve) where
import           Data.Foldable       (foldl')
import           Data.HashMap.Strict (HashMap, (!), (!?))
import qualified Data.HashMap.Strict as HM
import           Data.List           (elemIndex)
import           Data.Maybe          (fromJust)
import           Solution            (Solution (I))
import           StringUtils         (stringsToCharMap)
import           Text.Parsec         (Parsec, char, digit, many, many1, parse,
                                      (<|>))

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    (tileMap, instrs) = parseInput input
    part1 = solvePart1 tileMap instrs
    part2 = solvePart2 tileMap instrs

data Direction = DLeft | DRight deriving Show
data Instruction = Turn Direction | Move Integer deriving Show

type Pos = (Int, Int)
data Tile = Space | Wall deriving (Show, Eq)
type TileMap = HashMap Pos Tile

parseInput :: String -> (TileMap, [Instruction])
parseInput input = (parseTileMap tileStrs, parseInstructions $ instrStrs !! 1)
  where
    (tileStrs, instrStrs) = break (== "") (lines input)

-- Tile parsing

parseTileMap :: [String] -> TileMap
parseTileMap strs = HM.map charToTile $ HM.filter (/= ' ') $ HM.mapKeys (\(x, y) -> (x + 1, y + 1)) charMap
  where
    (charMap, _, _) = stringsToCharMap strs

charToTile :: Char -> Tile
charToTile '.' = Space
charToTile '#' = Wall
charToTile _   = undefined

-- Instruction parsing

parseInstructions :: String -> [Instruction]
parseInstructions str = case parse instrsParser "" str of
  Left pe      -> error $ show pe
  Right instrs -> instrs

instrsParser :: Parsec String () [Instruction]
instrsParser = many instrParser

instrParser :: Parsec String () Instruction
instrParser = turnParser <|> moveParser

turnParser :: Parsec String () Instruction
turnParser = do
  c <- char 'L' <|> char 'R'
  let direction = charToDir c
  return $ Turn direction
  where
    charToDir :: Char -> Direction
    charToDir 'L' = DLeft
    charToDir 'R' = DRight
    charToDir _   = undefined

moveParser :: Parsec String () Instruction
moveParser = Move . read <$> many1 digit

-- General

type WrapFun = TileMap -> Facing -> Pos -> Pos -> (Pos, Facing)

getPassword :: WrapFun -> TileMap -> [Instruction] -> Integer
getPassword wrapFun tileMap instrs = 1000 * toInteger y + 4 * toInteger x + facingScore
  where
    (Actor (x, y) facing) = executeInstrs wrapFun tileMap startActor instrs
    startActor = findStartActor tileMap
    facingScore = case facing of
      FRight -> 0
      FDown  -> 1
      FLeft  -> 2
      FUp    -> 3

findStartActor :: TileMap -> Actor
findStartActor tileMap = Actor {aPos = pos, aFacing = FRight}
  where
    pos = head [pos' | x <- [0 .. ], let pos' = (x, 1), tileMap !? pos' == Just Space]

data Facing = FRight | FDown | FLeft | FUp deriving (Eq, Show)

facings :: [Facing]
facings = [FRight, FDown, FLeft, FUp]

data Actor = Actor {
  aPos    :: Pos,
  aFacing :: Facing
}

turn :: Direction -> Facing -> Facing
turn dir facing = facings !! (i' `mod` 4)
  where
    i' = case dir of
      DLeft  -> i - 1
      DRight -> i + 1
    i = fromJust $ elemIndex facing facings

turnRight :: Facing -> Facing
turnRight = turn DRight

turnAround :: Facing -> Facing
turnAround = turnRight . turnRight

executeInstrs :: WrapFun -> TileMap -> Actor -> [Instruction] -> Actor
executeInstrs wrapFun tileMap = foldl' (executeInstr wrapFun tileMap)

executeInstr :: WrapFun -> TileMap -> Actor -> Instruction -> Actor
executeInstr wrapFun tileMap actor instr = case instr of
  Turn dir -> actor{aFacing = turn dir $ aFacing actor}
  Move n   -> actor{aPos = pos, aFacing = facing}
    where
      (pos, facing) = move wrapFun tileMap (aFacing actor) (aPos actor) n

move :: WrapFun -> TileMap -> Facing -> Pos -> Integer -> (Pos, Facing)
move _ _ facing pos 0 = (pos, facing)
move wrapFun tileMap facing pos n = case tileMap ! nextPos' of
  Space -> move wrapFun tileMap facing' nextPos' (n - 1)
  Wall  -> (pos, facing) -- Can't move any further
  where
    nextPos = moveDir pos facing
    (nextPos', facing') = wrapFun tileMap facing pos nextPos

moveDir :: Pos -> Facing -> Pos
moveDir (x, y) facing = case facing of
  FRight -> (x + 1, y)
  FDown  -> (x, y + 1)
  FLeft  -> (x - 1, y)
  FUp    -> (x, y - 1)

-- Part 1

solvePart1 :: TileMap -> [Instruction] -> Integer
solvePart1 = getPassword handleWrapping1

handleWrapping1 :: TileMap -> Facing -> Pos -> Pos -> (Pos, Facing)
handleWrapping1 tileMap facing _ nextPos
  | HM.member nextPos tileMap = (nextPos, facing)
  | otherwise = (moveUntilEdge tileMap facing' nextPos, facing)
  where
    facing' = turnAround facing

moveUntilEdge :: TileMap -> Facing -> Pos -> Pos
moveUntilEdge tileMap facing pos
  | not $ HM.member nextPos tileMap = pos
  | otherwise = moveUntilEdge tileMap facing nextPos
  where
    nextPos = moveDir pos facing

-- Part 2

solvePart2 :: TileMap -> [Instruction] -> Integer
solvePart2 = getPassword handleWrapping2

handleWrapping2 :: TileMap -> Facing -> Pos -> Pos -> (Pos, Facing)
handleWrapping2 tileMap facing pos nextPos
  | HM.member nextPos tileMap = (nextPos, facing)
  | otherwise = wrapCube pos facing

wrapCube :: Pos -> Facing -> (Pos, Facing)
wrapCube pos facing = case facing of
  FRight -> fromJust $ lookup pos rightMap
  FDown  -> fromJust $ lookup pos downMap
  FLeft  -> fromJust $ lookup pos leftMap
  FUp    -> fromJust $ lookup pos upMap
  where
    rightMap = sideMap f f' True FLeft ++ sideMap c c' False FUp ++ sideMap f' f True FLeft ++ sideMap a' a False FUp
    downMap = sideMap g g' False FDown ++ sideMap a a' False FLeft ++ sideMap c' c False FLeft
    leftMap = sideMap d' d True FRight ++ sideMap b' b False FDown ++ sideMap d d' True FRight ++ sideMap e' e False FDown
    upMap = sideMap b b' False FRight ++ sideMap e e' False FRight ++ sideMap g' g False FUp

    a  = [(x, 150) | x <- [51 .. 100]]
    a' = [(50, y)  | y <- [151 .. 200]]
    b  = [(x, 101) | x <- [1 .. 50]]
    b' = [(51, y)  | y <- [51 .. 100]]
    c  = [(100, y) | y <- [51 .. 100]]
    c' = [(x, 50)  | x <- [101 .. 150]]
    d  = [(1, y)   | y <- [101 .. 150]]
    d' = [(51, y)  | y <- [1 .. 50]]
    e  = [(x, 1)   | x <- [51 .. 100]]
    e' = [(1, y)   | y <- [151 .. 200]]
    f  = [(100, y) | y <- [101 .. 150]]
    f' = [(150, y) | y <- [1 .. 50]]
    g  = [(x, 200) | x <- [1 .. 50]]
    g' = [(x, 1)   | x <- [101 .. 150]]

sideMap :: [Pos] -> [Pos] -> Bool -> Facing -> [(Pos, (Pos, Facing))]
sideMap side1 side2 shouldReverse facing = zip side1 (addFacing side2' facing)
  where
    side2' = if shouldReverse then reverse side2 else side2

addFacing :: [Pos] -> Facing -> [(Pos, Facing)]
addFacing side facing = zip side $ repeat facing


