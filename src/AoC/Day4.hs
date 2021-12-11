module AoC.Day4 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (find, transpose, (\\))
import Data.List.Split (splitOn)

data Cell = Marked Int | Unmarked Int deriving (Show, Eq)

type Board = [[Cell]]

readInput :: [Char] -> ([Int], [Board])
readInput =
  bimap (map read . splitOn ",") (map (map (map (Unmarked . read) . words) . lines))
    . (\(d : b) -> (d, b))
    . splitOn "\n\n"

play :: Int -> Board -> Board
play draw = map (map (\case Unmarked x | x == draw -> Marked x; c -> c))

isWinner :: Board -> Bool
isWinner board = any isWinnerRow board || any isWinnerRow (transpose board)
  where
    isWinnerRow = all (\case Marked _ -> True; _ -> False)

playGame1 :: [Board] -> [Int] -> (Int, Board)
playGame1 boards = \case
  [] -> error "No more draws"
  (draw : rest) ->
    let boards' = map (play draw) boards
     in case find isWinner boards' of
          Just board -> (draw, board)
          Nothing -> playGame1 boards' rest

part1 :: String -> String
part1 input =
  let (draws, boards) = readInput input
      (wDraw, wBoard) = playGame1 boards draws
   in show $ wDraw * sum [x | row <- wBoard, Unmarked x <- row]

playGame2 :: [[(Int, Board)]] -> [Board] -> [Int] -> [(Int, Board)]
playGame2 wins boards = \case
  [] -> error "No more draws"
  (draw : rest) -> case boards of
    [] -> head wins
    _ ->
      let boards' = map (play draw) boards; winners = filter isWinner boards'
       in playGame2 (map (draw,) winners : wins) (boards' \\ winners) rest

part2 :: String -> String
part2 input =
  let (draws, boards) = readInput input
   in case playGame2 [] boards draws of
        [] -> error "No winning boards"
        [(wDraw, wBoard)] -> show $ wDraw * sum [x | row <- wBoard, Unmarked x <- row]
        _ -> error "Multiple winning boards"
