module AoC.Day07 where

import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

readInput :: String -> [Int]
readInput = map read . splitOn ","

solve :: ([Int] -> Int -> Int) -> String -> String
solve f input =
  show $ f input' $ minimumBy (comparing (f input')) [minimum input' .. maximum input']
  where
    input' = readInput input

part1 :: String -> String
part1 = solve $ \input dest -> sum [abs (dest - pos) | pos <- input]

part2 :: String -> String
part2 = solve $ \input dest -> sum [dist * (dist + 1) `div` 2 | pos <- input, let dist = abs (dest - pos)]
