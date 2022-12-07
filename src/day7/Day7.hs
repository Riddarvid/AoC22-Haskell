module Day7 (solve) where
import           Data.HashMap.Lazy (HashMap, (!))
import qualified Data.HashMap.Lazy as HM
import           Solution          (Solution (I))

type Dir = (HashMap String FileDir)

data FileDir = Dir Dir | File Integer
  deriving (Show)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    root = parseRoot $ lines input
    (usedSpace, dirSizes) = fileDirSize $ Dir root
    part1 = sumBelowLimit dirSizes 100000
    part2 = smallestAbove dirSizes $ neededSpace usedSpace

-- Parse file tree

parseRoot :: [String] -> Dir
parseRoot instrs = parseRoot' instrs [] HM.empty

parseRoot' :: [String] -> [String] -> Dir -> Dir
parseRoot' [] _ dir = dir
parseRoot' (instr:instrs) path dir | tokens !! 1 == "cd" = parseRoot' instrs path' dir
  where
    tokens = words instr
    dest = tokens !! 2
    path' = case dest of
      "/"   -> []
      ".."  -> tail path
      dest' -> dest' : path
parseRoot' (instr:instrs) path dir | tokens !! 1 == "ls" = parseRoot' instrs' path dir'
  where
    tokens = words instr
    (lsDir, instrs') = parseLs instrs
    dir' = insertByPath path lsDir dir
parseRoot' _ _ _ = error "Parse error"

parseLs :: [String] -> (Dir, [String])
parseLs xs = (HM.fromList (map parseFileDir fileDirs), drop (length fileDirs) xs)
  where
    fileDirs = takeWhile (\s -> head s /= '$') xs

parseFileDir :: String -> (String, FileDir)
parseFileDir line
  | head tokens == "dir" = (tokens !! 1, Dir HM.empty)
  | otherwise = (tokens !! 1, File $ read $ head tokens)
  where
    tokens = words line

insertByPath :: [String] -> Dir -> Dir -> Dir
insertByPath path = insertByPath' (reverse path)

insertByPath' :: [String] -> Dir -> Dir -> Dir
insertByPath' [] toInsert _ = toInsert
insertByPath' (name:path) toInsert dir = HM.insert name (Dir updated) dir
  where
    updated = case dir ! name of
      (File _)   -> error "Can't cd into file"
      (Dir dir') -> insertByPath' path toInsert dir'

-- Part 1

sumBelowLimit :: [Integer] -> Integer -> Integer
sumBelowLimit dirSizes n = sum $ filter (<= n) dirSizes

-- Part 2

neededSpace :: Integer -> Integer
neededSpace usedSpace = 30000000 - freeSpace
  where
    freeSpace = 70000000 - usedSpace

smallestAbove :: [Integer] -> Integer -> Integer
smallestAbove dirSizes n = minimum $ filter (>= n) dirSizes

-- General

-- The first element of the tuple is the total size of the object, the second is the size of all subdirs
fileDirSize :: FileDir -> (Integer, [Integer])
fileDirSize (File size) = (size, [])
fileDirSize (Dir dir) = (dirSize, dirSize : subDirSizes)
  where
    subFileDirs = HM.elems dir
    dirSize = foldr (\fileDir n -> n + fst (fileDirSize fileDir)) 0 subFileDirs
    subDirSizes = foldr (\fileDir xs -> snd (fileDirSize fileDir) ++ xs) [] subFileDirs
