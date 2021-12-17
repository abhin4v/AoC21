module AoC.Day08 where

import AoC.Utils (intsToNumber, pairify)
import Control.Monad ((>=>))
import Data.Bifunctor (bimap)
import Data.List (find, intersect, permutations, sort, sortOn, (\\))
import Data.List.Extra (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Tuple.Extra (both)

digitToSegments :: Map.Map Int String
digitToSegments =
  Map.fromList
    [ (0, "abcefg"),
      (1, "cf"),
      (2, "acdeg"),
      (3, "acdfg"),
      (4, "bcdf"),
      (5, "abdfg"),
      (6, "abdefg"),
      (7, "acf"),
      (8, "abcdefg"),
      (9, "abcdfg")
    ]

readInput :: String -> [([String], [String])]
readInput = map (both words . pairify . splitOn " | ") . lines

part1 :: String -> String
part1 input =
  show $
    length
      [ digit
        | digits <- map snd $ readInput input,
          digit <- digits,
          length digit `elem` map (length . (digitToSegments Map.!)) [1, 7, 4, 8]
      ]

segmentsToDigit :: Map.Map String Int
segmentsToDigit = Map.fromList . map swap . Map.toList $ digitToSegments

solve :: [String] -> [String] -> Int
solve patterns = intsToNumber . map ((segmentsToDigit Map.!) . sort . map (finalMapping Map.!))
  where
    finalMapping =
      Map.fromList $
        foldl
          eliminatePossibilities
          []
          [ map (bimap (foldl1 intersect (patternsWithSize 5) `intersect`) ("adg" `intersect`)) uniqueSegmentsPossibilities,
            uniqueSegmentsPossibilities,
            [(concatMap ((digitToSegments Map.! 8) \\) $ patternsWithSize 6, "cde")],
            [(head $ patternsWithSize 7, digitToSegments Map.! 8)]
          ]

    uniqueSegmentsPossibilities =
      foldl (\poss segs -> foldl pairDiff (head . patternsWithSize $ length segs, segs) poss : poss) [] $
        map (digitToSegments Map.!) [1, 7, 4, 8]
    patternsWithSize n = [p | p <- patterns, length p == n]
    pairDiff (s1l, s1r) (s2l, s2r) = (s1l \\ s2l, s1r \\ s2r)

    eliminatePossibilities mapping =
      (mapping <>)
        . map (both head)
        . filter (\(from, _) -> length from == 1)
        . map (`pairDiff` unzip mapping)

part2 :: String -> String
part2 = show . sum . map (uncurry solve) . readInput

translate :: [(Char, Char)] -> String -> Maybe Int
translate mapping = traverse (`lookup` mapping) >=> flip Map.lookup segmentsToDigit . sort

deduceMapping :: [String] -> [(Char, Char)]
deduceMapping patterns =
  fromJust
    . find (isJust . for patterns . translate)
    . map (zip "abcdefg")
    $ permutations "abcdefg"

part2Alt :: String -> String
part2Alt input =
  show $
    sum
      [ intsToNumber $ fromJust $ traverse (translate $ deduceMapping $ sortOn length patterns) output
        | (patterns, output) <- readInput input
      ]
