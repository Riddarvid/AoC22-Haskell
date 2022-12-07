module Day7 (solve) where
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Solution          (Solution)

data Instruction = ILs | ICd String | IDir String | IFile String Integer
  deriving (Show)

data Dir = Root (HashMap String FileDir) | NotRoot Dir (HashMap String FileDir)

data FileDir = Dir Dir | File Integer

solve :: String -> [Instruction]
solve input = instrs
  where
    instrs = parseInstrs input

parseInstrs :: String -> [Instruction]
parseInstrs input = map parseInstr $ lines input

parseInstr :: String -> Instruction
parseInstr line
  | head (words line) == "$" = parseCommandInstr $ tail $ words line
  | otherwise = parseFileDirInstr $ words line

parseCommandInstr :: [String] -> Instruction
parseCommandInstr line
  | head line == "cd" = ICd (line !! 1)
  | otherwise = ILs

parseFileDirInstr :: [String] -> Instruction
parseFileDirInstr line
  | head line == "dir" = IDir (line !! 1)
  | otherwise = IFile (line !! 1) (read (head line))

{-
parseInput :: String -> Dir
parseInput input = parseCommands (lines input) (Root HM.empty)

parseCommands :: [String] -> Dir -> Dir
parseCommands [] dir         = dir
parseCommands (cmd:cmds) dir = parseCommands cmds' dir'
  where
    (dir', cmds') = parseCommand cmd cmds dir
-}
