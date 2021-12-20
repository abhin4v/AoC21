module AoC.Day20 where

import AoC.Utils (pairify)
import qualified Data.Array.BitArray as BA
import Data.Bifunctor (bimap)
import Data.Bits.Bitwise (fromListBE)
import Data.Bool (bool)
import Data.List.Extra (chunksOf, splitOn)
import Data.Tuple.Extra (both)

type Image = (Bool, BA.BitArray (Int, Int))

type EnhancementAlgo = BA.BitArray Int

readInput :: Int -> String -> (EnhancementAlgo, Image)
readInput iterations =
  bimap
    ((\l -> BA.listArray (0, length l - 1) l) . map toPixel)
    ((False,) . makeArray . map (map toPixel) . lines)
    . pairify
    . splitOn "\n\n"
  where
    toPixel = \case '#' -> True; '.' -> False; _ -> error "Invalid pixel"
    makeArray g =
      let width = 2 * (iterations + 1) + length (head g)
          height = 2 * (iterations + 1) + length g
          widthPadding = replicate (iterations + 1) False
          heightPadding = replicate (iterations + 1) $ replicate width False
       in BA.listArray ((0, 0), (width - 1, height - 1))
            . concat
            $ heightPadding <> map ((widthPadding <>) . (<> widthPadding)) g <> heightPadding

printImage :: Image -> IO ()
printImage (_, image) =
  putStrLn
    . unlines
    . map (concatMap (bool " . " " # "))
    . chunksOf (succ $ fst $ snd $ BA.bounds image)
    . BA.elems
    $ image

enhance :: EnhancementAlgo -> Image -> Image
enhance algo (spacePixel, image) =
  let (width, height) = both succ $ snd $ BA.bounds image
      spacePixel' = algo BA.! fromListBE (replicate 9 spacePixel)
      isBoundary (x, y) = x == 0 || x == width - 1 || y == 0 || y == height - 1
   in (spacePixel',) $
        BA.listArray
          ((0, 0), (width - 1, height - 1))
          [ if isBoundary (x, y) then spacePixel' else algo BA.! index
            | (x, y) <- BA.indices image,
              let square = [(x', y') | x' <- [x - 1, x, x + 1], y' <- [y - 1, y, y + 1]],
              let index = fromListBE $ map (image BA.!) square
          ]

litPixelCount :: Image -> Int
litPixelCount = BA.popCount . snd

solve :: Int -> String -> String
solve iterations input =
  let (algo, image) = readInput iterations input
   in show $ litPixelCount $ iterate (enhance algo) image !! iterations

part1 :: String -> String
part1 = solve 2

part2 :: String -> String
part2 = solve 50
