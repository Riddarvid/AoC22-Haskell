{-# LANGUAGE InstanceSigs #-}
module Day13 (solve) where
import           Data.List        (sort)
import           Data.List.Utils  (split)
import           Solution         (Solution (I))
import           Text.Parsec      (Parsec, between, digit, many1, parse, sepBy)
import           Text.Parsec.Char (char)
import           Text.Parsec.Prim ((<|>))
import           Utils            (indexOf)

solve :: String -> (Solution, Solution)
solve input = (I part1, I part2)
  where
    pairs = parsePairs input
    part1 = sum $ map fst $ filter (\(_, (a, b)) -> a < b) $ zip [1 ..] pairs
    div1 = PList [PList [PInt 2]]
    div2 = PList [PList [PInt 6]]
    allPackets = sort $ div1 : div2 : foldr (\(packet1, packet2) acc -> packet1 : packet2 : acc) [] pairs
    part2 = toInteger (indexOf div1 allPackets) * toInteger (indexOf div2 allPackets)

data Packet = PList [Packet] | PInt Integer
  deriving Eq

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (PInt m) (PInt n)             = compare m n
  compare left@(PInt _) right@(PList _) = compare (PList [left]) right
  compare left@(PList _) right@(PInt _) = compare left (PList [right])
  compare (PList []) (PList []) = EQ
  compare (PList []) (PList _) = LT
  compare (PList _) (PList []) = GT
  compare (PList (left : lefts)) (PList (right : rights)) = case compare left right of
    EQ     -> compare (PList lefts) (PList rights)
    result -> result

-- Parse pairs

parsePairs :: String -> [(Packet, Packet)]
parsePairs input = map parsePair $ split [""] $ lines input

parsePair :: [String] -> (Packet, Packet)
parsePair [packetStr1, packetStr2] = (parsePacket packetStr1, parsePacket packetStr2)
parsePair _                    = error "Should receive exactly two packets"

-- Parse packets

parsePacket :: String -> Packet
parsePacket str = case parse packetParser "" str of
  Left err     -> error $ show err
  Right packet -> packet

packetParser :: Parsec String () Packet
packetParser = numberParser <|> listParser

numberParser :: Parsec String () Packet
numberParser = PInt . read <$> many1 digit

listParser :: Parsec String () Packet
listParser = PList <$> between (char '[') (char ']') (sepBy packetParser (char ','))
