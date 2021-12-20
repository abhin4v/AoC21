module AoC.Day03 where

import AoC.Utils (binaryToNumber)
import Control.Arrow ((>>>))
import Data.List (transpose)

incCounts :: (Int, Int) -> Char -> (Int, Int)
incCounts (z, o) = \case
  '0' -> (z + 1, o)
  '1' -> (z, o + 1)
  _ -> error "invalid input"

counts1 :: [String] -> [(Int, Int)]
counts1 = map (foldl incCounts (0, 0)) . transpose

part1 :: String -> String
part1 input = show $ binaryToNumber gamma * binaryToNumber epsilon
  where
    input' = lines input
    gamma = map (\(z, o) -> if z > o then '0' else '1') $ counts1 input'
    epsilon = map (\(z, o) -> if z < o then '0' else '1') $ counts1 input'

counts2 :: [String] -> Int -> (Int, Int)
counts2 input i = foldl incCounts (0, 0) $ map (!! i) input

rating :: ((Int, Int) -> Char) -> [String] -> String
rating f =
  (0,)
    >>> iterate (\(i, inp) -> (i + 1, filter ((== f (counts2 inp i)) . (!! i)) inp))
    >>> dropWhile (\(_, inp) -> length inp /= 1)
    >>> head
    >>> snd
    >>> head

part2 :: String -> String
part2 input = show $ binaryToNumber oxygenRating * binaryToNumber co2Rating
  where
    input' = lines input
    oxygenRating = flip rating input' $ \(z, o) -> if z > o then '0' else '1'
    co2Rating = flip rating input' $ \(z, o) -> if o < z then '1' else '0'
