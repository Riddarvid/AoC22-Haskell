module Solution (Solution(..)) where

data Solution = I Integer | S String

instance Show Solution where
  show (I n) = show n
  show (S s) = s

