module AoC.Day12 where

import AoC.Utils (pairify)
import Data.Char (isAsciiUpper)
import qualified Data.Graph.Wrapper as G
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing, mapMaybe)
import qualified Data.Set as Set
import Data.Tuple (swap)

type Cave = String

type CaveMap = G.Graph Cave Cave

readInput :: String -> CaveMap
readInput input =
  let edges = map (pairify . splitOn "-") . lines $ input
      vertices = nub $ map fst edges <> map snd edges
   in G.fromVerticesEdges (map (\x -> (x, x)) vertices) $ edges <> map swap edges

calcPathCount :: CaveMap -> (seen -> Cave -> Maybe seen) -> seen -> Int
calcPathCount graph modifySeen = go "start"
  where
    go "end" _ = 1
    go cave seen =
      sum
        . map (uncurry go)
        . mapMaybe (\cave' -> (cave',) <$> modifySeen seen cave')
        . filter (/= "start")
        $ G.successors graph cave

isBigCave :: Cave -> Bool
isBigCave = all isAsciiUpper

calcPathCount1 :: CaveMap -> Int
calcPathCount1 graph = calcPathCount graph modifySeen $ Set.singleton "start"
  where
    modifySeen seen cave
      | isBigCave cave = Just seen
      | Set.notMember cave seen = Just $ Set.insert cave seen
      | otherwise = Nothing

part1 :: String -> String
part1 = show . calcPathCount1 . readInput

calcPathCount2 :: CaveMap -> Int
calcPathCount2 graph = calcPathCount graph modifySeen (Nothing, Set.singleton "start")
  where
    modifySeen (twiceSeen, onceSeen) cave
      | isBigCave cave = Just (twiceSeen, onceSeen)
      | Set.notMember cave onceSeen = Just (twiceSeen, Set.insert cave onceSeen)
      | isNothing twiceSeen = Just (Just cave, onceSeen)
      | otherwise = Nothing

part2 :: String -> String
part2 = show . calcPathCount2 . readInput
