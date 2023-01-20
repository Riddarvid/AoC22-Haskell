module StringUtils (getInts, getIntegers, getIntsNeg, getIntegersNeg, stringsToCharMap, showGrid) where

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Data.Set          (Set)
import qualified Data.Set          as Set
import           Text.Regex.PCRE   (AllTextMatches (getAllTextMatches), (=~))

intRegex :: String
intRegex = "\\d+"

getInts :: String -> [Int]
getInts input = map read $ getAllTextMatches $ input =~ intRegex

getIntegers :: String -> [Integer]
getIntegers input = map read $ getAllTextMatches $ input =~ intRegex

negIntRegex :: String
negIntRegex = "-?\\d+"

getIntsNeg :: String -> [Int]
getIntsNeg input = map read $ getAllTextMatches $ input =~ negIntRegex

getIntegersNeg :: String -> [Integer]
getIntegersNeg input = map read $ getAllTextMatches $ input =~ negIntRegex

type IntPos = (Int, Int)

stringsToCharMap :: [String] -> (HashMap IntPos Char, Int, Int)
stringsToCharMap input = (HM.fromList mapList, maxX, maxY)
  where
    charList = concat input
    maxY = length input - 1
    maxX = maximum (map length input) - 1
    mapList = zip [(x, y) | y <- [0 .. maxY], x <- [0 .. (length (input !! y) - 1)]] charList

showGrid :: (Ord a, Enum a, Foldable t) => t (a, a) -> a -> a -> a -> a -> String
showGrid placed minX maxX minY maxY = unlines $ map (showLayer placed minX maxX) [minY .. maxY]

showLayer :: (Ord a, Enum a, Foldable t) => t (a, a) -> a -> a -> a -> String
showLayer placed minX maxX layer = [showPos placed (x, layer) | x <- [minX .. maxX]]

showPos :: (Ord a, Foldable t) => t (a, a) -> (a, a) -> Char
showPos placed pos
  | pos `elem` placed = '#'
  | otherwise = '.'
