module AoC.Day17 where

import AoC.Utils (pairify)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (both, snd3)

type Pos = (Int, Int)

type Vel = (Int, Int)

type Trench = ((Int, Int), (Int, Int))

calcPathToTrench :: Trench -> Vel -> Maybe [Pos]
calcPathToTrench ((minX, maxX), (minY, maxY)) =
  (\path -> if snd $ last path then Just $ map fst path else Nothing)
    . map (\(p, _, t) -> (p, t))
    . takeWhile snd3
    . map (\i -> (i, beforeTrenchEnd i, inTrench i))
    . move (0, 0)
  where
    move (x, y) (vx, vy) = (x, y) : move (x + vx, y + vy) (vx - signum vx, vy - 1)
    inTrench (x, y) = minX <= x && x <= maxX && minY <= y && y <= maxY
    beforeTrenchEnd (x, y) = x <= maxX && minY <= y

highestPos :: Trench -> Vel -> Maybe Int
highestPos trench = fmap (maximum . map snd) . calcPathToTrench trench

possibleVelocities :: Trench -> [Vel]
possibleVelocities ((_, maxX), (minY, _)) =
  [(vx, vy) | vx <- [1 .. maxX], vy <- [minY .. negate minY]]

maximumPossibleHeight :: Trench -> Int
maximumPossibleHeight trench =
  maximum $ mapMaybe (highestPos trench) $ possibleVelocities trench

readInput :: String -> Trench
readInput =
  both (both read . pairify . splitOn ".." . drop 2)
    . pairify
    . splitOn ", "
    . drop 13

part1 :: String -> String
part1 = show . maximumPossibleHeight . readInput

possibleInitialVelocityCount :: Trench -> Int
possibleInitialVelocityCount trench =
  length $ mapMaybe (calcPathToTrench trench) $ possibleVelocities trench

part2 :: String -> String
part2 = show . possibleInitialVelocityCount . readInput
