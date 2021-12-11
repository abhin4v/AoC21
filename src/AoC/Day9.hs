module AoC.Day9 where

import Data.List (sortOn)
import qualified Data.Set as Set
import Math.Geometry.Grid (Grid (indices, neighbours))
import Math.Geometry.Grid.Square (RectSquareGrid, rectSquareGrid)

type Input = [[Int]]

type Point = (Int, Int)

readInput :: String -> Input
readInput = map (map (read . (: []))) . lines

createGrid :: Input -> RectSquareGrid
createGrid input = rectSquareGrid (length input) (length $ head input)

pointHeight :: Input -> Point -> Int
pointHeight input (x, y) = input !! x !! y

-- Low points are points with height lower than any of neighbour points
lowPoints :: Input -> [Point]
lowPoints input =
  let grid = createGrid input
      points = indices grid
      pointNeighbours = map (\p -> (p, neighbours grid p)) points
   in map fst $ filter (\(p, ns) -> all (\n -> pointHeight input p < pointHeight input n) ns) pointNeighbours

part1 :: String -> String
part1 input = show $ sum [1 + pointHeight input' p | p <- lowPoints input']
  where
    input' = readInput input

-- basin points are neighbours of low points which not of height 9
calcBasin :: RectSquareGrid -> Input -> Set.Set Point -> Point -> Set.Set Point
calcBasin grid input basin i =
  let basin' = Set.insert i basin
      ns = [i' | i' <- neighbours grid i, Set.notMember i' basin', pointHeight input i' /= 9]
   in foldl (calcBasin grid input) basin' ns

-- Find the three largest basins and multiply their sizes together
part2 :: String -> String
part2 input =
  show
    . product
    . map length
    . take 3
    . sortOn (negate . length)
    . map (calcBasin (createGrid input') input' Set.empty)
    $ lowPoints input'
  where
    input' = readInput input
