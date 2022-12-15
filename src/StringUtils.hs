module StringUtils (getInts, getIntegers, getIntsNeg, getIntegersNeg, stringsToCharMap) where

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
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

type Pos = (Int, Int)

stringsToCharMap :: [String] -> (HashMap Pos Char, Int, Int)
stringsToCharMap input = (HM.fromList mapList, maxX, maxY)
  where
    charList = concat input
    maxY = length input - 1
    maxX = length (head input) - 1
    mapList = zip [(x, y) | y <- [0 .. maxY], x <- [0 .. maxX]] charList
