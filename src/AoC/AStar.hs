{-# LANGUAGE MultiParamTypeClasses #-}

module AoC.AStar (astar) where

import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.OrdPSQ as PQ
import qualified Data.Set as Set

-- | A* algorithm: Finds a path from initial node to goal node using a heuristic function.
astar ::
  (Ord a, Ord b, Num b) =>
  -- | The start node
  a ->
  -- | The goal node
  a ->
  -- | The function to get the next nodes and their
  -- costs from a given node
  (a -> [(a, b)]) ->
  -- | The heuristic function to estimate the cost of
  -- going from a give node to the target node
  (a -> a -> b) ->
  -- | Nothing if no path found. Else @Just (path cost, path)@
  Maybe (b, [a])
astar initNode goalNode nextNode hueristic =
  astar'
    (PQ.singleton initNode (hueristic initNode goalNode) 0)
    Set.empty
    (Map.singleton initNode 0)
    Map.empty
  where
    -- pq: open set, seen: closed set, tracks: tracks of states
    astar' pq seen gscore tracks
      -- If open set is empty then search has failed. Return Nothing
      | PQ.null pq = Nothing
      -- If goal node reached then construct the path from the tracks and node
      | node == goalNode = Just (gcost, findPath tracks node)
      -- If node has already been seen then discard it and continue
      | Set.member node seen = astar' pq' seen gscore tracks
      -- Else expand the node and continue
      | otherwise = astar' pq'' seen' gscore' tracks'
      where
        -- Find the node with min f-cost
        (node, _, gcost) = fromJust $ PQ.findMin pq

        -- Delete the node from open set
        pq' = PQ.deleteMin pq

        -- Add the node to the closed set
        seen' = Set.insert node seen

        -- Find the successors (with their g and h costs) of the node
        -- which have not been seen yet
        successors =
          filter
            ( \(s, g, _) ->
                not (Set.member s seen')
                  && ( not (s `Map.member` gscore)
                         || g < (fromJust . Map.lookup s $ gscore)
                     )
            )
            $ successorsAndCosts node gcost

        -- Insert the successors in the open set
        pq'' = foldl' (\q (s, g, h) -> PQ.insert s (g + h) g q) pq' successors

        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors

        -- Insert the tracks of the successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors

    -- Finds the successors of a given node and their costs
    successorsAndCosts node gcost =
      map (\(s, g) -> (s, gcost + g, hueristic s goalNode)) . nextNode $ node

    -- Constructs the path from the tracks and last node
    findPath tracks node =
      if Map.member node tracks
        then findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
        else [node]
