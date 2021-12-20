module Main where

import qualified AoC.Day01 as Day01
import qualified AoC.Day02 as Day02
import qualified AoC.Day03 as Day03
import qualified AoC.Day04 as Day04
import qualified AoC.Day05 as Day05
import qualified AoC.Day06 as Day06
import qualified AoC.Day07 as Day07
import qualified AoC.Day08 as Day08
import qualified AoC.Day09 as Day09
import qualified AoC.Day10 as Day10
import qualified AoC.Day11 as Day11
import qualified AoC.Day12 as Day12
import qualified AoC.Day13 as Day13
import qualified AoC.Day14 as Day14
import qualified AoC.Day15 as Day15
import qualified AoC.Day16 as Day16
import qualified AoC.Day17 as Day17
import qualified AoC.Day18 as Day18
import qualified AoC.Day20 as Day20
import System.Environment (getArgs)

solutions :: [(Int, [String -> String])]
solutions =
  [ (1, [Day01.part1, Day01.part2]),
    (2, [Day02.part1, Day02.part2]),
    (3, [Day03.part1, Day03.part2]),
    (4, [Day04.part1, Day04.part2]),
    (5, [Day05.part1, Day05.part2]),
    (6, [Day06.part1, Day06.part2]),
    (7, [Day07.part1, Day07.part2]),
    (8, [Day08.part1, Day08.part2]),
    (9, [Day09.part1, Day09.part2]),
    (10, [Day10.part1, Day10.part2]),
    (11, [Day11.part1, Day11.part2]),
    (12, [Day12.part1, Day12.part2]),
    (13, [Day13.part1, Day13.part2]),
    (14, [Day14.part1, Day14.part2]),
    (15, [Day15.part1, Day15.part2]),
    (16, [Day16.part1, Day16.part2]),
    (17, [Day17.part1, Day17.part2]),
    (18, [Day18.part1, Day18.part2]),
    (20, [Day20.part1, Day20.part2])
  ]

main :: IO ()
main = do
  getArgs >>= \case
    [day, part, file] -> do
      let day' = read day
          part' = read part
      case lookup day' solutions of
        Nothing -> putStrLn "No solution for that day"
        Just sols | part' `elem` [1, 2] -> do
          input <- readFile file
          let partSolution = sols !! (part' - 1)
          putStrLn $ partSolution input
        _ -> putStrLn "No solution for that part"
    _ -> putStrLn "Usage: aoc21 <day> <part> <input_file>"
