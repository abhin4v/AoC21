module AoC.Day6 where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.List (group, sort)
import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

step :: MV.STVector s Int -> Int -> ST s ()
step counts day = do
  dayCount <- MV.unsafeRead counts day
  MV.unsafeWrite counts (day + 9) dayCount
  MV.unsafeModify counts (+ dayCount) (day + 7)

solve :: Int -> String -> String
solve days input = runST $ do
  let input' = map read . splitOn "," $ input
  counts <- MV.replicate (days + 9) 0
  forM_ (group $ sort input') $ \l -> MV.unsafeWrite counts (head l) $ length l
  forM_ [0 .. days - 1] $ step counts
  finalCounts <- V.freeze $ MV.unsafeSlice days 9 counts
  return $ show $ sum $ V.toList finalCounts

part1 :: String -> String
part1 = solve 80

part2 :: String -> String
part2 = solve 256
