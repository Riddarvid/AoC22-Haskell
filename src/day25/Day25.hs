module Day25 (solve, fromSnafu, toSnafu) where
import           Data.Char (digitToInt, intToDigit)
import           Solution  (Solution (S))

solve :: String -> (Solution, Solution)
solve input = (S part1, S "Final challenge solved!")
  where
    snafuInputs = lines input
    part1 = solvePart1 snafuInputs

type Snafu = String

solvePart1 :: [Snafu] -> Snafu
solvePart1 snafus = toSnafu $ sum $ map fromSnafu snafus

fromSnafu :: Snafu -> Integer
fromSnafu snafu = fromSnafu' (reverse snafu) 1

fromSnafu' :: Snafu -> Integer -> Integer
fromSnafu' [] _          = 0
fromSnafu' (c:cs) factor = factor * n + fromSnafu' cs (factor * 5)
  where
    n = case c of
      '=' -> (-2)
      '-' -> (-1)
      c'  -> toInteger $ digitToInt c'

toSnafu :: Integer -> Snafu
toSnafu n = toSnafu' n ""

toSnafu' :: Integer -> Snafu -> Snafu
toSnafu' 0 snafu = snafu
toSnafu' n snafu = toSnafu' n' (c:snafu)
  where
    remainder = n `mod` 5
    remainder' = if remainder > 2 then remainder - 5 else remainder
    c = case remainder' of
      (-2) -> '='
      (-1) -> '-'
      i    -> intToDigit $ fromInteger i
    n' = (n - remainder') `div` 5
