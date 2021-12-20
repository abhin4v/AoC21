module AoC.Day15 where

import AoC.AStar (astar)
import Data.Char (digitToInt)
import qualified Data.Vector.Unboxed as Vector
import Math.Geometry.Grid (neighbours)
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

readInput :: String -> (Int, Int, Vector.Vector Int)
readInput input = let
  ls = lines input
  in (length $ head ls, length ls, Vector.fromList . concatMap (map digitToInt) $ ls)

findLowestPathCost :: RectSquareGrid -> (Int, Int) -> ((Int, Int) -> Int) -> Maybe Int
findLowestPathCost grid goal risk =
  fst <$> astar (0, 0) goal (map (\n -> (n, risk n)) . neighbours grid) (const $ const 0)

part1 :: String -> String
part1 input' =
  let (width, height, input) = readInput input'
      goal = (width - 1, height - 1)
      grid = rectSquareGrid width height
      risk (x, y) = input `Vector.unsafeIndex` (y * width + x)
   in case findLowestPathCost grid goal risk of
        Just cost -> show cost
        Nothing -> error "No path found"

part2 :: String -> String
part2 input' =
  let (width, height, input) = readInput input'
      scale = 5
      goal = (scale * width - 1, scale * height - 1)
      grid = rectSquareGrid (scale * width) (scale * height)
      risk (x, y)
        | x < width && y < height = input `Vector.unsafeIndex` (y * width + x)
        | otherwise =
          let ((sx, x'), (sy, y')) = (x `quotRem` width, y `quotRem` height)
              risk' = sy + sx + risk (x', y')
           in if risk' > 9 then risk' - 9 else risk'
   in case findLowestPathCost grid goal risk of
        Just cost -> show cost
        Nothing -> error "No path found"
