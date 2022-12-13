module Utils (indexOf) where

indexOf :: Eq a => a -> [a] -> Int
indexOf target xs = indexOf' target $ zip [1 ..] xs

indexOf' :: Eq a => a -> [(Int, a)] -> Int
indexOf' _ [] = error "Element not in list"
indexOf' target ((i, x) : xs)
  | target == x = i
  | otherwise = indexOf' target xs
