module StringUtils (getInts, stringsToCharMap) where

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import           Text.Regex.PCRE   (AllTextMatches (getAllTextMatches), (=~))

intRegex :: String
intRegex = "\\d+"

getInts :: String -> [Integer]
getInts input = map read $ getAllTextMatches $ input =~ intRegex

type Pos = (Int, Int)

stringsToCharMap :: [String] -> HashMap Pos Char
stringsToCharMap input = HM.fromList mapList
  where
    charList = concat input
    mapList = zip [(x, y) | y <- [0 .. length input - 1], x <- [0 .. length (head input) - 1]] charList
