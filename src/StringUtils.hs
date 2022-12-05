module StringUtils (getInts) where

import           Text.Regex.PCRE (AllTextMatches (getAllTextMatches), (=~))

intRegex :: String
intRegex = "\\d+"

getInts :: String -> [Integer]
getInts input = map read $ getAllTextMatches $ input =~ intRegex
