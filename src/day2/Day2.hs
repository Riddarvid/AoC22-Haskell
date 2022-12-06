module Day2 (solve) where
import           Data.Foldable (find)
import           Data.Maybe
import           Solution

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    part1 = totalScore $ parseStrats parseStrategy1 input
    part2 = totalScore $ parseStrats parseStrategy2 input

-- Score

totalScore :: [Strategy] -> Integer
totalScore = foldl (\n strat -> n + score strat) 0

score :: Strategy -> Integer
score strat@(_, me) = shapeScore + resultScore
  where
    shapeScore = case me of
      Rock     -> 1
      Paper    -> 2
      Scissors -> 3
    resultScore = case result strat of
      Win  -> 6
      Draw -> 3
      Loss -> 0

-- Game rules

data Shape = Rock | Paper | Scissors
  deriving (Show, Eq)

beatsArr :: [Strategy]
beatsArr = [(Rock, Scissors), (Scissors, Paper), (Paper, Rock)]

beats :: Strategy -> Bool
beats strat = strat `elem` beatsArr

type Strategy = (Shape, Shape)

data Result = Win | Loss | Draw

result :: Strategy -> Result
result (opponent, me)
  | beats (me, opponent) = Win
  | beats (opponent, me) = Loss
  | otherwise = Draw

shapeByResult :: Shape -> Result -> Shape
shapeByResult opp Draw = opp
shapeByResult opp res = case res of
  Loss -> lossRes
  Win  -> winRes
  where
    (_, lossRes) = fromJust $ find (\strat -> opp == fst strat) beatsArr
    (winRes, _) = fromJust $ find (\strat -> opp == snd strat) beatsArr

-- Parsing

parseStrats :: (String -> Strategy) -> String -> [Strategy]
parseStrats strategyParser input = map strategyParser $ lines input


parseStrategy1 :: String -> Strategy
parseStrategy1 line = case words line of
  [sym1, sym2] -> (symToShape sym1, symToShape sym2)
  _            -> error "Parse error"

parseStrategy2 :: String -> Strategy
parseStrategy2 line = case words line of
  [sym1, sym2] -> (opponentShape, shapeByResult opponentShape gameResult)
    where
      opponentShape = symToShape sym1
      gameResult = symToResult sym2
  _            -> error "Parse error"

symToShape :: String -> Shape
symToShape "A" = Rock
symToShape "B" = Paper
symToShape "C" = Scissors
symToShape "X" = Rock
symToShape "Y" = Paper
symToShape "Z" = Scissors
symToShape _   = error "Parse error"

symToResult :: String -> Result
symToResult "X" = Loss
symToResult "Y" = Draw
symToResult "Z" = Win
symToResult _   = error "Parse error"
