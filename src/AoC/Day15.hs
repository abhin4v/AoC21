module AoC.Day15 where

import AoC.AStar (astar)
import Data.Char (digitToInt)
import qualified Data.Vector as Vector
import Math.Geometry.Grid (Grid (distance, neighbours))
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

readInput :: String -> Vector.Vector (Vector.Vector Int)
readInput = Vector.fromList . map (Vector.fromList . map digitToInt) . lines

findLowestPathCost :: RectSquareGrid -> (Int, Int) -> ((Int, Int) -> Int) -> Maybe Int
findLowestPathCost grid goal risk =
  fst <$> astar (0, 0) goal (map (\n -> (n, risk n)) . neighbours grid) (distance grid)

part1 :: String -> String
part1 input' =
  let input = readInput input'
      height = length input
      width = length $ Vector.head input
      goal = (width - 1, height - 1)
      grid = rectSquareGrid width height
      risk (x, y) = input Vector.! y Vector.! x
   in case findLowestPathCost grid goal risk of
        Just cost -> show cost
        Nothing -> error "No path found"

part2 :: String -> String
part2 input' =
  let input = readInput input'
      height = length input
      width = length $ Vector.head input
      scale = 5
      goal = (scale * width - 1, scale * height - 1)
      grid = rectSquareGrid (scale * width) (scale * height)
      risk (x, y)
        | x < width && y < height = input Vector.! y Vector.! x
        | otherwise =
          let ((sx, x'), (sy, y')) = (x `divMod` width, y `divMod` height)
              risk' = sy + sx + risk (x', y')
           in if risk' > 9 then risk' `mod` 9 else risk'
   in case findLowestPathCost grid goal risk of
        Just cost -> show cost
        Nothing -> error "No path found"
