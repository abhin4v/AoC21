{-# LANGUAGE MultiParamTypeClasses #-}

module AoC.AStar (astar) where

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set

-- | A* algorithm: Finds a path from initial node to goal node using a heuristic function.
astar ::
  (Ord a, Ord b, Num b) =>
  -- | The start node
  a ->
  -- | The goal node
  a ->
  -- | The function to get the next nodes and their costs from a given node
  (a -> [(a, b)]) ->
  -- | The heuristic function to estimate the cost of going from a give node to the target node
  (a -> a -> b) ->
  -- | Nothing if no path found. Else @Just (path cost, path)@
  Maybe (b, [a])
astar initNode goalNode nextNode hueristic =
  astar'
    (PQ.singleton (hueristic initNode goalNode) (initNode, 0))
    Set.empty
    (Map.singleton initNode 0)
    Map.empty
  where
    astar' !pq !seen !gscore tracks
      | PQ.null pq = Nothing
      | node == goalNode = Just (gcost, findPath tracks node)
      | Set.member node seen = astar' pq' seen gscore tracks
      | otherwise = astar' pq'' seen' gscore' tracks'
      where
        (node, gcost) = snd $ PQ.findMin pq
        pq' = PQ.deleteMin pq
        seen' = Set.insert node seen
        successors =
          [ (s, g', hueristic s goalNode)
            | (s, g) <- nextNode node,
              Set.notMember s seen',
              let g' = gcost + g,
              s `Map.notMember` gscore || g' < gscore Map.! s
          ]

        pq'' = foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors
        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors

    findPath tracks node =
      if Map.member node tracks
        then findPath tracks (tracks Map.! node) ++ [node]
        else [node]
