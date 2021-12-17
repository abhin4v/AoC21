module AoC.Day13 where

import AoC.Utils (pairify)
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.List.Extra (splitOn)
import qualified Data.Set as Set
import Data.Tuple.Extra (both)

type Points = Set.Set (Int, Int)

data Axis = X | Y

type Fold = (Axis, Int)

readInput :: String -> (Points, [Fold])
readInput =
  bimap
    (Set.fromList . map (both read . pairify . splitOn ",") . lines)
    (map (bimap toAxis read . pairify . splitOn "=" . drop 11) . lines)
    . pairify
    . splitOn "\n\n"
  where
    toAxis = \case "x" -> X; "y" -> Y; _ -> error "Invalid axis"

foldPaper :: Points -> Fold -> Points
foldPaper points (axis, foldCoord) =
  flip Set.map points $ setter $ \x -> foldCoord - abs (foldCoord - x)
  where
    setter = case axis of X -> first; Y -> second

part1 :: String -> String
part1 input =
  let (points, folds) = readInput input
   in show . length . foldPaper points $ head folds

showPaper :: Points -> String
showPaper points =
  let len = Set.findMax $ Set.map snd points
      wid = Set.findMax $ Set.map fst points
      foldedPaper =
        [ [if (x, y) `Set.member` points then "██" else "  " | x <- [0 .. wid]]
          | y <- [0 .. len]
        ]
   in unlines $ map concat foldedPaper

part2 :: String -> String
part2 input =
  let (points, folds) = readInput input
   in showPaper $ foldl foldPaper points folds
