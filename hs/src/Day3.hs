{-|
Module:         Day3
Description:    <https://adventofcode.com/2021/day/3 Day 3: Binary Diagnostic>
-}
module Day3 (day3a, day3b) where

import Control.Monad (foldM)
import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.List (group, maximumBy, minimumBy, sort, transpose)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

bin :: (Num a) => String -> Either String a
bin = foldM f 0 where
    f x '0' = Right $! 2 * x
    f x '1' = Right $! 2 * x + 1
    f _ c = Left $ "bad char " ++ [c]

common, uncommon :: (Ord a) => [a] -> a
common xs = head $ maximumBy (comparing length) $ group $ sort xs
uncommon xs = head $ minimumBy (comparing length) $ group $ sort xs

day3a :: Text -> Either String Int
day3a input = (*) <$> bin gamma <*> bin epsilon where
    nums = T.unpack <$> T.lines input
    gamma = common <$> transpose nums
    epsilon = chr . xor 1 . ord <$> gamma

day3b :: Text -> Either String Int
day3b input = (*) <$> bin o2 <*> bin co2 where
    nums = T.unpack <$> T.lines input
    scrub _ xs | any null xs = []
    scrub f xs = x : scrub f [ys | y:ys <- xs, y == x] where x = f $ head <$> xs
    o2 = scrub common nums
    co2 = scrub uncommon nums
