module Combinatorics (permutations) where

permutations :: Int -> [a] -> [[a]]
permutations 0 _      = [[]]
permutations _ []     = error "Not enough elements"
permutations n (x:xs) = [x:ys | yss <- permutations (n - 1) (removeOne xs), ys <- yss]

removeOne :: [a] -> [[a]]
removeOne xs = [start ++ tail end | n <- [1 .. length xs - 1], let (start, end) = splitAt n xs]
