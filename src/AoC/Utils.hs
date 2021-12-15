module AoC.Utils where

import Data.Char (digitToInt, isDigit)
import Data.List (group, sort)
import qualified Text.ParserCombinators.ReadP as P

freq :: Ord a => [a] -> [(a, Int)]
freq = map (\l -> (head l, length l)) . group . sort

pairify :: [b] -> (b, b)
pairify [x, y] = (x, y)
pairify _ = error "pairify: list must have exactly two elements"

sliding :: Int -> [a] -> [[a]]
sliding s = \case xs | length xs >= s -> take s xs : sliding s (drop 1 xs); _ -> []

binaryToDec :: String -> Int
binaryToDec = foldl (\n d -> n * 2 + digitToInt d) 0

intsToNumber :: [Int] -> Int
intsToNumber = foldl (\n d -> n * 10 + d) 0

intP :: P.ReadP Int
intP = read <$> P.many1 (P.satisfy isDigit)
