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
    maxX = length (head input) - 1
    mapList = zip [(x, y) | y <- [0 .. maxY], x <- [0 .. maxX]] charList

type IntegerPos = (Integer, Integer)

showGrid :: Set IntegerPos -> Integer -> Integer -> Integer -> Integer -> String
showGrid placed minX maxX minY maxY = unlines $ map (showLayer placed minX maxX) [maxY, maxY - 1 .. minY]

showLayer :: Set IntegerPos -> Integer -> Integer -> Integer -> String
showLayer placed minX maxX layer = [showPos placed (x, layer) | x <- [minX .. maxX]]

showPos :: Set IntegerPos -> IntegerPos -> Char
showPos placed pos
  | Set.member pos placed = '#'
  | otherwise = '.'
