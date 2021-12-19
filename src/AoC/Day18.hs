module AoC.Day18 where

import AoC.Utils (intP)
import Control.Applicative ((<|>))
import Data.Either.Extra (isLeft, isRight, mapLeft, mapRight)
import Data.Maybe (fromMaybe)
import qualified Text.ParserCombinators.ReadP as P

data SNum = P SNum SNum | I Int deriving (Eq)

instance Show SNum where
  show = \case
    P a b -> "[" ++ show a ++ "," ++ show b ++ "]"
    I i -> show i

pairP :: P.ReadP SNum
pairP = P.between (P.char '[') (P.char ']') $ P <$> sNumP <* P.char ',' <*> sNumP

sNumP :: P.ReadP SNum
sNumP = P.choice [I <$> intP, pairP]

parse :: String -> SNum
parse s = case P.readP_to_S sNumP s of
  [(n, "")] -> n
  _ -> error "parse error"

type Zipper = (SNum, [Either SNum SNum])

toZipper :: SNum -> Zipper
toZipper n = (n, [])

goLeft :: Zipper -> Maybe Zipper
goLeft = \case (P l r, ctxs) -> Just (l, Left r : ctxs); _ -> Nothing

goRight :: Zipper -> Maybe Zipper
goRight = \case (P l r, ctxs) -> Just (r, Right l : ctxs); _ -> Nothing

goUp :: Zipper -> Maybe Zipper
goUp = \case
  (l, Left r : ctxs) -> Just (P l r, ctxs)
  (r, Right l : ctxs) -> Just (P l r, ctxs)
  (_, []) -> Nothing

goToRoot :: Zipper -> Zipper
goToRoot = \case (num, []) -> (num, []); z -> maybe z goToRoot $ goUp z

modifyOnce :: (Zipper -> Maybe Zipper) -> SNum -> Maybe SNum
modifyOnce go = fmap (fst . goToRoot) . modifyOnce' go . toZipper
  where
    modifyOnce' f z = f z <|> (goLeft z >>= modifyOnce' f) <|> (goRight z >>= modifyOnce' f)

explode :: SNum -> Maybe SNum
explode = modifyOnce $ \case
  (P (I l) (I r), ctxs@[_, _, _, _]) -> Just (I 0, modifyNumOnRight r $ modifyNumOnLeft l ctxs)
  _ -> Nothing
  where
    modifyNumOnLeft = modify isRight mapRight (goTillNum goRight)
    modifyNumOnRight = modify isLeft mapLeft (goTillNum goLeft)
    goTillNum f = \case n@(I _, _) -> n; z -> maybe z (goTillNum f) $ f z

    modify matcher modifier finder x ctxs = case ctxs of
      [] -> []
      ctx : rest | matcher ctx -> modifier (findAndModify finder x) ctx : rest
      ctx : rest -> ctx : modify matcher modifier finder x rest

    findAndModify finder x = fst . goToRoot . modifyInt (+ x) . finder . toZipper
    modifyInt f (I i, ctxs) = (I (f i), ctxs)

split :: SNum -> Maybe SNum
split = modifyOnce $ \case
  (I x, ctxs) | x >= 10 -> Just (P (I $ x `div` 2) (I $ x - (x `div` 2)), ctxs)
  _ -> Nothing

reduce :: SNum -> SNum
reduce n = let n' = fromMaybe n $ explode n <|> split n in if n == n' then n else reduce n'

add :: SNum -> SNum -> SNum
add n1 n2 = reduce $ P n1 n2

magnitude :: SNum -> Int
magnitude = \case I i -> i; P l r -> 3 * magnitude l + 2 * magnitude r

readInput :: String -> [SNum]
readInput = map parse . lines

part1 :: String -> String
part1 = show . magnitude . foldl1 add . readInput

part2 :: String -> String
part2 input' = show $ maximum [magnitude $ add x y | x <- input, y <- input, x /= y]
  where
    input = readInput input'
