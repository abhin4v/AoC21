module AoC.Day11 where

import Control.Monad (foldM)
import Control.Monad.State.Strict (State, evalState, execState, modify')
import Data.Traversable (for)
import Math.Geometry.Grid (Grid (neighbours))
import Math.Geometry.Grid.Octagonal (RectOctGrid, rectOctGrid)

data OctoState = Energy Int | Flashed deriving (Eq)

instance Show OctoState where
  show (Energy n) | n > 9 = "* "
  show (Energy n) = show n <> " "
  show Flashed = "✖ "

newtype Input = Input [[OctoState]] deriving (Eq)

instance Show Input where
  show (Input xs) = unlines $ map (unwords . map show) xs

readInput :: String -> Input
readInput input = Input $ map (map parse) $ lines input
  where
    parse = Energy . read . (: [])

createGrid :: Input -> RectOctGrid
createGrid (Input input) = rectOctGrid (length input) (length $ head input)

step1 :: Input -> State Int Input
step1 (Input input) = pure $ Input $ map (map (\(Energy e) -> Energy $ e + 1)) input

step2' :: RectOctGrid -> Input -> State Int Input
step2' grid (Input input) =
  fmap Input . for (zip [0 ..] input) $ \(i, row) ->
    for (zip [0 ..] row) $ \(j, en) ->
      flip calcNewEneryLevel en $ map (\(x, y) -> input !! x !! y) $ neighbours grid (i, j)

calcNewEneryLevel :: [OctoState] -> OctoState -> State Int OctoState
calcNewEneryLevel es = \case
  Energy n | n > 9 -> modify' (+ 1) >> pure Flashed
  Energy n -> pure $ Energy $ n + length [() | Energy n' <- es, n' > 9]
  Flashed -> pure Flashed

step2 :: RectOctGrid -> Input -> State Int Input
step2 grid input = do
  input' <- step2' grid input
  if input == input' then pure input else step2 grid input'

step3 :: Input -> State Int Input
step3 (Input input) =
  pure $ Input [[if Flashed == en then Energy 0 else en | en <- row] | row <- input]

step :: RectOctGrid -> Input -> State Int Input
step grid input = step1 input >>= step2 grid >>= step3

simulate :: Int -> RectOctGrid -> Input -> Int
simulate stepCount grid input =
  flip execState 0 . foldM (const . step grid) input $ [1 .. stepCount]

part1 :: String -> String
part1 input = show $ simulate 100 (createGrid input') input'
  where
    input' = readInput input

simulateTillSync' :: Int -> RectOctGrid -> Input -> State Int Int
simulateTillSync' stepCount grid input
  | isSynced input = pure stepCount
  | otherwise = step grid input >>= simulateTillSync' (stepCount + 1) grid
  where
    isSynced (Input inp) = all (== Energy 0) $ concat inp

simulateTillSync :: RectOctGrid -> Input -> Int
simulateTillSync grid input = evalState (simulateTillSync' 0 grid input) 0

part2 :: String -> String
part2 input = show $ simulateTillSync (createGrid input') input'
  where
    input' = readInput input
