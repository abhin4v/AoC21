module AoC.Day10 where

import Data.List (find, sort)
import Data.Maybe (fromJust, isNothing, listToMaybe, mapMaybe)
import qualified Text.ParserCombinators.ReadP as P

data Delim = C Char | N deriving (Eq)

instance Show Delim where
  show (C c) = [c]
  show N = "*"

data Chunk = Chunk Delim [Chunk] Delim deriving (Eq)

instance Show Chunk where
  show (Chunk d cs d') = show d ++ concatMap show cs ++ show d'

matchingDelim :: [(Char, Char)]
matchingDelim =
  [ ('(', ')'),
    ('[', ']'),
    ('{', '}'),
    ('<', '>')
  ]

parseStartDelim :: P.ReadP Delim
parseStartDelim =
  P.look >>= \case
    c : _ -> case lookup c matchingDelim of
      Nothing -> P.pfail
      Just _ -> C <$> P.char c
    _ -> P.pfail

parseEndDelim :: P.ReadP Delim
parseEndDelim =
  P.look >>= \case
    c : _ -> case find (\(_, v) -> c == v) matchingDelim of
      Nothing -> P.pfail
      Just _ -> C <$> P.char c
    "" -> pure N

parseChunk :: P.ReadP Chunk
parseChunk = Chunk <$> parseStartDelim <*> P.many parseChunk <*> parseEndDelim

parse :: String -> [Chunk]
parse s = case P.readP_to_S (P.many1 parseChunk <* P.eof) s of
  [(res, "")] -> res
  _ -> error "parse error"

findCorruptedDelim :: Chunk -> Maybe Delim
findCorruptedDelim = \case
  Chunk (C start) cs end
    | fmap C (lookup start matchingDelim) == Just end || end == N ->
      listToMaybe $ mapMaybe findCorruptedDelim cs
  Chunk (C _) _ end -> Just end
  _ -> error "Impossible"

corruptionPoints :: [(Delim, Int)]
corruptionPoints =
  [ (C ')', 3),
    (C ']', 57),
    (C '}', 1197),
    (C '>', 25137)
  ]

part1 :: String -> String
part1 =
  show
    . sum
    . fromJust
    . traverse (`lookup` corruptionPoints)
    . concatMap (mapMaybe findCorruptedDelim . parse)
    . lines

autoCompletionNeeded :: Chunk -> String
autoCompletionNeeded = \case
  Chunk (C start) cs N ->
    concatMap autoCompletionNeeded cs <> [fromJust $ lookup start matchingDelim]
  Chunk _ cs _ -> concatMap autoCompletionNeeded cs

calcScore :: Int -> String -> Int
calcScore score = \case
  "" -> score
  (c : rest) -> calcScore (5 * score + charScore c) rest
  where
    charScore = \case
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      _ -> error "Impossible"

part2 :: String -> String
part2 =
  show
    . (\score -> score !! (length score `div` 2))
    . sort
    . filter (/= 0)
    . map (calcScore 0 . autoCompletionNeeded)
    . concat
    . filter (all (isNothing . findCorruptedDelim))
    . map parse
    . lines
