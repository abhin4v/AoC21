module AoC.Day01 where

import AoC.Utils (sliding)

readInput :: String -> [Int]
readInput = map read . lines

solve :: Ord a => [a] -> Int
solve ns = length $ filter id $ zipWith (<) ns $ tail ns

part1 :: String -> String
part1 = show . solve . readInput

part2 :: String -> String
part2 = show . solve . map sum . sliding 3 . readInput
