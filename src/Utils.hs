module Utils (indexOf, showMap) where
import           Data.Map (Map)
import qualified Data.Map as Map

indexOf :: Eq a => a -> [a] -> Int
indexOf target xs = indexOf' target $ zip [1 ..] xs

indexOf' :: Eq a => a -> [(Int, a)] -> Int
indexOf' _ [] = error "Element not in list"
indexOf' target ((i, x) : xs)
  | target == x = i
  | otherwise = indexOf' target xs

showMap :: (Show a, Show b) => Map a b -> String
showMap myMap = "\n" ++ unlines (map (\(a, b) -> show a ++ " -> " ++ show b) $ Map.toAscList myMap)
