{-|
Module:         Day11
Description:    <https://adventofcode.com/2021/day/11 Day 11: Dumbo Octopus>
-}
module Day11 (day11) where

import Control.Monad (guard)
import Data.Char (digitToInt, isDigit)
import Data.Ix (inRange)
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Text (Text)
import qualified Data.Text as T (all, length, lines, unpack)
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V (accum, all, findIndex, length, map, fromList)

step :: (Num a, Ord a, Unbox a) => Int -> Vector a -> Maybe (Int, Vector a)
step width = step' 0 . V.map (+ 1) where
    step' n v
      | Just i <- V.findIndex (> 9) v = step' (n + 1) $ V.accum f v
          [ (x' + y' * width, x == x' && y == y')
          | let (y, x) = i `divMod` width
          , x' <- filter (inRange (0, width - 1)) [x - 1..x + 1]
          , y' <- filter (inRange (0, V.length v `div` width - 1)) [y - 1..y + 1]
          ]
      | V.all (< 0) v = Nothing
      | otherwise = Just (n, V.map (max 0) v)
    f _ True = -1
    f a _ = if a < 0 then a else a + 1

day11 :: Text -> Maybe (Int, Int)
day11 input = do
    input'@(input0 :| inputs) <- nonEmpty $ T.lines input
    guard $ all (T.all isDigit) input'
    let width = T.length input0
    guard $ all ((== width) . T.length) inputs
    let flashes = unfoldr (step width) $ V.fromList $ digitToInt <$> concatMap T.unpack input'
    pure (sum $ take 100 flashes, length flashes + 1)
