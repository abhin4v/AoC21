module AoC.Day16 where

import AoC.Utils (binaryToNumber)
import Control.Applicative ((<|>))
import Data.List.Extra (trimEnd)
import qualified Text.ParserCombinators.ReadP as P

hexToBin :: String -> String
hexToBin = concatMap go
  where
    go = \case
      '0' -> "0000"
      '1' -> "0001"
      '2' -> "0010"
      '3' -> "0011"
      '4' -> "0100"
      '5' -> "0101"
      '6' -> "0110"
      '7' -> "0111"
      '8' -> "1000"
      '9' -> "1001"
      'A' -> "1010"
      'B' -> "1011"
      'C' -> "1100"
      'D' -> "1101"
      'E' -> "1110"
      'F' -> "1111"
      c -> error $ "invalid hexadecimal character: " <> show c

data Packet = Literal Int Int | Operator Int Int [Packet] deriving (Show)

bit :: P.ReadP Char
bit = P.char '1' <|> P.char '0'

bits :: Int -> P.ReadP String
bits n = P.count n bit

num :: Int -> P.ReadP Int
num n = binaryToNumber <$> P.count n bit

literal :: Int -> P.ReadP Packet
literal version = Literal version <$> go []
  where
    go acc =
      bit >>= \case
        '1' -> bits 4 >>= go . (: acc)
        '0' -> binaryToNumber . concat . reverse . (: acc) <$> bits 4
        _ -> error "invalid literal"

operator :: Int -> Int -> P.ReadP Packet
operator version typeId = do
  lengthTypeId <- bit
  Operator version typeId <$> case lengthTypeId of
    '0' -> do
      totalLength <- num 15
      s <- P.count totalLength bit
      let [(packets, "")] = P.readP_to_S (P.many1 packet <* P.eof) s
      pure packets
    '1' -> do
      subPacketCount <- num 11
      P.count subPacketCount packet
    _ -> error "invalid operator"

packet :: P.ReadP Packet
packet = do
  version <- num 3
  typeId <- num 3
  case typeId of
    4 -> literal version
    _ -> operator version typeId

parse :: String -> Packet
parse s =
  let [(pkt, rest)] = P.readP_to_S packet $ hexToBin s
   in if binaryToNumber rest == 0
        then pkt
        else error "invalid packet"

addVersions :: Packet -> Int
addVersions = \case
  Literal ver _ -> ver
  Operator ver _ packets -> ver + sum (map addVersions packets)

interpret :: Packet -> Int
interpret = \case
  Literal _ val -> val
  Operator _ 0 packets -> sum $ map interpret packets
  Operator _ 1 packets -> product $ map interpret packets
  Operator _ 2 packets -> minimum $ map interpret packets
  Operator _ 3 packets -> maximum $ map interpret packets
  Operator _ 5 [p1, p2] -> if interpret p1 > interpret p2 then 1 else 0
  Operator _ 6 [p1, p2] -> if interpret p1 < interpret p2 then 1 else 0
  Operator _ 7 [p1, p2] -> if interpret p1 == interpret p2 then 1 else 0
  Operator {} -> error "invalid operator"

readInput :: String -> String
readInput = trimEnd

part1 :: String -> String
part1 = show . addVersions . parse . readInput

part2 :: String -> String
part2 = show . interpret . parse . readInput
