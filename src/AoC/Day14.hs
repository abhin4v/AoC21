{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module AoC.Day14 where

import AoC.Utils (freq, pairify, sliding)
import Data.Bifunctor (second)
import Data.Foldable (Foldable (toList))
import Data.List (sortOn)
import Data.List.Extra (splitOn)
import qualified Data.Map.Strict as Map

type Template = String

type Subs = Map.Map Template Char

readInput :: String -> (Template, Subs)
readInput =
  second (Map.fromList . map (second head . pairify . splitOn " -> ") . lines)
    . pairify
    . splitOn "\n\n"

solve :: Int -> String -> Map.Map Template Char -> Int
solve stepCount template subs =
  let pairCounts = go stepCount $ Map.fromList $ freq $ toList $ sliding 2 template
      charCounts =
        foldl
          (\m (pair, count) -> foldl (insert count) m pair)
          (foldl (insert 1) Map.empty [head template, last template])
          $ Map.toList pairCounts
      charCountList = sortOn snd $ Map.toList charCounts
   in (snd (last charCountList) - snd (head charCountList)) `div` 2
  where
    go 0 pairCounts = pairCounts
    go stepCount' pairCounts =
      go (stepCount' - 1) $
        foldl
          (\counts (pair, count) -> foldl (insert count) counts $ expandPair pair)
          Map.empty
          $ Map.toList pairCounts

    insert v m k = Map.insertWith (+) k v m

    expandPair = \case
      s@[a, b] -> maybe [s] (\c -> [[a, c], [c, b]]) $ Map.lookup s subs
      _ -> error "expandPair: invalid input"

part1 :: String -> String
part1 = show . uncurry (solve 10) . readInput

part2 :: String -> String
part2 = show . uncurry (solve 40) . readInput
