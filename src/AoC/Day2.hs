module AoC.Day2 where

import Control.Applicative ((<|>))
import qualified Text.ParserCombinators.ReadP as P
import AoC.Utils (intP)

data Move = F Int | D Int | U Int deriving (Show)

move :: P.ReadP Move
move = forward <|> up <|> down
  where
    forward = F <$> (P.string "forward " *> intP)
    up = U <$> (P.string "up " *> intP)
    down = D <$> (P.string "down " *> intP)

parse :: String -> Move
parse = fst . head . P.readP_to_S (move <* P.eof)

execMove1 :: (Int, Int) -> Move -> (Int, Int)
execMove1 (h, d) = \case F i -> (h + i, d); U i -> (h, d - i); D i -> (h, d + i)

execMove2 :: (Int, Int, Int) -> Move -> (Int, Int, Int)
execMove2 (h, d, a) = \case F i -> (h + i, d + a * i, a); U i -> (h, d, a - i); D i -> (h, d, a + i)

readInput :: String -> [Move]
readInput = map parse . lines

part1 :: String -> String
part1 input =
  let moves = readInput input
      (hor, dep) = foldl execMove1 (0, 0) moves
   in show $ hor * dep

part2 :: String -> String
part2 input =
  let moves = readInput input
      (hor, dep, _) = foldl execMove2 (0, 0, 0) moves
   in show $ hor * dep
