{-|
Module:         Day6
Description:    <https://adventofcode.com/2021/day/6 Day 6: Lanternfish>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day6 (day6a, day6b) where

import Common (readEntire)
import Data.Array.Unboxed (IArray, UArray, (!), accumArray, bounds, elems, listArray, range)
import Data.Char (isSpace)
import Data.Semigroup (stimes)
import Data.Text (Text)
import qualified Data.Text as T (dropAround, splitOn)
import qualified Data.Text.Read as T (decimal)

newtype Matrix a e = Matrix { fromMatrix :: a (Int, Int) e }

instance (IArray a e, Num e) => Semigroup (Matrix a e) where
    Matrix x <> Matrix y = Matrix $ listArray ((i0, j0), (i1, j1))
      [ sum $ zipWith (*) [x ! (i, k0) | k0 <- range (k00, k01)] [y ! (k1, j) | k1 <- range (k10, k11)]
      | (i, j) <- range ((i0, j0), (i1, j1))
      ] where
        ((i0, k00), (i1, k01)) = bounds x
        ((k10, j0), (k11, j1)) = bounds y

step :: (IArray a e, Num e) => Matrix a e
step = Matrix $ accumArray (const id) 0 ((0, 0), (8, 8)) $
    ((6, 0), 1) : ((8, 0), 1) : [((i, i + 1), 1) | i <- [0..7]]

day6 :: Matrix UArray Int -> Text -> Either String Int
day6 matrix input = do
    nums <- mapM (readEntire T.decimal) $ T.splitOn "," $ T.dropAround isSpace input
    pure $ sum $ elems $ fromMatrix $ matrix <> Matrix
        (accumArray (+) 0 ((0, 0), (8, 0)) [((i, 0), 1) | i <- nums])

day6a :: Text -> Either String Int
day6a = day6 $ stimes (80 :: Int) step

day6b :: Text -> Either String Int
day6b = day6 $ stimes (256 :: Int) step
