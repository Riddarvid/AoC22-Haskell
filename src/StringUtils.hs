module StringUtils (getInts, stringsToCharMap) where

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Text.Regex.PCRE   (AllTextMatches (getAllTextMatches), (=~))

intRegex :: String
intRegex = "\\d+"

getInts :: String -> [Integer]
getInts input = map read $ getAllTextMatches $ input =~ intRegex

type Pos = (Int, Int)

stringsToCharMap :: [String] -> (HashMap Pos Char, Int, Int)
stringsToCharMap input = (HM.fromList mapList, maxX, maxY)
  where
    charList = concat input
    maxY = length input - 1
    maxX = length (head input) - 1
    mapList = zip [(x, y) | y <- [0 .. maxY], x <- [0 .. maxX]] charList
