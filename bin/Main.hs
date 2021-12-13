module Main where

import qualified AoC.Day1 as Day1
import qualified AoC.Day10 as Day10
import qualified AoC.Day11 as Day11
import qualified AoC.Day12 as Day12
import qualified AoC.Day13 as Day13
import qualified AoC.Day2 as Day2
import qualified AoC.Day3 as Day3
import qualified AoC.Day4 as Day4
import qualified AoC.Day5 as Day5
import qualified AoC.Day6 as Day6
import qualified AoC.Day7 as Day7
import qualified AoC.Day8 as Day8
import qualified AoC.Day9 as Day9
import System.Environment (getArgs)

solutions :: [(Int, [String -> String])]
solutions =
  [ (1, [Day1.part1, Day1.part2]),
    (2, [Day2.part1, Day2.part2]),
    (3, [Day3.part1, Day3.part2]),
    (4, [Day4.part1, Day4.part2]),
    (5, [Day5.part1, Day5.part2]),
    (6, [Day6.part1, Day6.part2]),
    (7, [Day7.part1, Day7.part2]),
    (8, [Day8.part1, Day8.part2]),
    (9, [Day9.part1, Day9.part2]),
    (10, [Day10.part1, Day10.part2]),
    (11, [Day11.part1, Day11.part2]),
    (12, [Day12.part1, Day12.part2]),
    (13, [Day13.part1, Day13.part2])
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
