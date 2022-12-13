{-# LANGUAGE InstanceSigs #-}
module Day13 (solve) where
import           Data.Char       (isDigit)
import           Data.List       (sort)
import           Data.List.Utils (split)
import           Solution        (Solution (I))
import           Utils           (indexOf)

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

-- Parsing

parsePairs :: String -> [(Packet, Packet)]
parsePairs input = map parsePair $ split [""] $ lines input

parsePair :: [String] -> (Packet, Packet)
parsePair [packetStr1, packetStr2] = (parsePacket packetStr1, parsePacket packetStr2)
parsePair _                    = error "Should receive exactly two packets"

-- Below could probably be done better. Might improve.

parsePacket :: String -> Packet
parsePacket str = snd $ parsePacket' str

parsePacket' :: String -> (String, Packet)
parsePacket' str
  | head str == '[' = parsePList str
  | otherwise = parsePInt str

parsePList :: String -> (String, Packet)
parsePList str = (str', PList packets)
  where
    (str', packets) = parsePacketList $ tail str

parsePacketList :: String -> (String, [Packet])
parsePacketList str
  | head str == ']' = (tail str, [])
  | head str == ',' = parsePacketList $ tail str
  | otherwise = (str'', packet : packets)
  where
    (str', packet) = parsePacket' str
    (str'', packets) = parsePacketList str'

parsePInt :: String -> (String, Packet)
parsePInt str = (str', PInt (read intStr))
  where
    (intStr, str') = span isDigit str
