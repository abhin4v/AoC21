module AoC.Day05 where

import AoC.Utils
import Data.List (sort)
import Data.List.Extra (splitOn)
import Data.Ratio (numerator, (%))

type Point = (Int, Int)

type Line = (Point, Point)

parse :: String -> Line
parse = pairify . map (pairify . map read . splitOn ",") . splitOn " -> "

readInput :: String -> [Line]
readInput = map parse . lines

horVertLines :: [Line] -> [Line]
horVertLines = map sortPoints . filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)
  where
    sortPoints ((x1, y1), (x2, y2)) = pairify $ sort [(x1, y1), (x2, y2)]

horVertPoints :: [Line] -> [Point]
horVertPoints = concatMap line2Points . horVertLines
  where
    line2Points ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

solve :: [Point] -> Int
solve = length . map fst . filter ((> 1) . snd) . freq

part1 :: String -> String
part1 = show . solve . horVertPoints . readInput

diagLine2Points :: Line -> [Point]
diagLine2Points ((x1, y1), (x2, y2)) =
  [ (x, y)
    | let slope = numerator $ (y2 - y1) % (x2 - x1),
      let constant = y1 - slope * x1,
      x <- if x1 < x2 then [x1 .. x2] else [x2 .. x1],
      let y = slope * x + constant
  ]

diagLines :: [Line] -> [Line]
diagLines = filter (\((x1, y1), (x2, y2)) -> x1 /= x2 && y1 /= y2)

part2 :: String -> String
part2 input =
  let ls = readInput input
   in show $ solve $ horVertPoints ls <> concatMap diagLine2Points (diagLines ls)
