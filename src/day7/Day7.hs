module Day7 (solve) where
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM
import           Solution          (Solution (I))

data Instruction = ILs [FileDesc] | ICd String
  deriving (Show)

data FileDesc = FDir String | FFile String Integer
  deriving (Show)

type Dir = (HashMap String FileDir)

data FileDir = Dir Dir | File Integer
  deriving (Show)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    inputInstrs = parseInstrs input
    root = parseRoot inputInstrs
    part1 = sumBelowLimit root 100000
    part2 = smallestAbove root $ neededSpace root

-- Parse instructions

parseInstrs :: String -> [Instruction]
parseInstrs input = parseInstrs' $ lines input

parseInstrs' :: [String] -> [Instruction]
parseInstrs' [] = []
parseInstrs' (x:xs) = instr : parseInstrs' xs'
  where
    (instr, xs') = parseInstr x xs

parseInstr :: String -> [String] -> (Instruction, [String])
parseInstr line xs
  | tokens !! 1 == "cd" = (ICd (tokens !! 2), xs)
  | otherwise = parseLs xs
  where
    tokens = words line

parseLs :: [String] -> (Instruction, [String])
parseLs xs = (ILs (map parseFileDesc fileDirs), drop (length fileDirs) xs)
  where
    fileDirs = takeWhile (\s -> head s /= '$') xs

parseFileDesc :: String -> FileDesc
parseFileDesc line
  | head tokens == "dir" = FDir (tokens !! 1)
  | otherwise = FFile (tokens !! 1) (read (head tokens))
  where
    tokens = words line

-- Parse file tree

parseRoot :: [Instruction] -> Dir
parseRoot instrs = parseRoot' instrs [] HM.empty

parseRoot' :: [Instruction] -> [String] -> Dir -> Dir
--parseRoot' instrs path dir | trace ("Remaining instrs: " ++ show instrs ++ "\nCurrent path: " ++ show path ++ "\nCurrent tree: " ++ show dir ++ "\n") False = undefined
parseRoot' [] _ dir = dir
parseRoot' (ICd dest:instrs) path dir = parseRoot' instrs path' dir
  where
    path' = case dest of
      "/"   -> []
      ".."  -> tail path
      dest' -> dest' : path
parseRoot' (ILs files:instrs) path dir = parseRoot' instrs path dir'
  where
    dir' = insertByPath path (newDir files) dir

insertByPath :: [String] -> Dir -> Dir -> Dir
insertByPath path = insertByPath' (reverse path)

insertByPath' :: [String] -> Dir -> Dir -> Dir
insertByPath' [] toInsert _ = toInsert
insertByPath' (name:path) toInsert dir = HM.insert name (Dir updated) dir
  where
    updated = case dir ! name of
      (File _)   -> error "Can't cd into file"
      (Dir dir') -> insertByPath' path toInsert dir'

newDir :: [FileDesc] -> Dir
newDir descs = HM.fromList $ map descToMap descs

descToMap :: FileDesc -> (String, FileDir)
descToMap (FDir name)       = (name, Dir HM.empty)
descToMap (FFile name size) = (name, File size)

-- Part 1

sumBelowLimit :: Dir -> Integer -> Integer
sumBelowLimit dir n = sum $ filter (<= n) $ dirSizes (Dir dir)

-- Part 2

neededSpace :: Dir -> Integer
neededSpace dir = 30000000 - freeSpace
  where
    usedSpace = totalSize (Dir dir)
    freeSpace = 70000000 - usedSpace

smallestAbove :: Dir -> Integer -> Integer
smallestAbove dir n = minimum $ filter (>= n) $ dirSizes (Dir dir)

-- General

dirSizes :: FileDir -> [Integer]
dirSizes (File _) = []
dirSizes fileDir@(Dir dir) = dirSize : subDirSizes
  where
    dirSize = totalSize fileDir
    subDirs = HM.elems dir
    subDirSizes = foldr (\subDir ls -> dirSizes subDir ++ ls) [] subDirs

totalSize :: FileDir -> Integer
totalSize (File size) = size
totalSize (Dir dir)   = HM.foldr (\fileDir n -> totalSize fileDir + n) 0 dir
