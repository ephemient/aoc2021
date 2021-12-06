{-|
Module:         Day6
Description:    <https://adventofcode.com/2021/day/6 Day 6: Lanternfish>
-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Day6 (day6a, day6b) where

import Common (readEntire)
import Data.Char (isSpace)
import qualified Data.IntMap.Strict as IntMap (assocs, elems, fromListWith)
import Data.Text (Text)
import qualified Data.Text as T (dropAround, splitOn)
import qualified Data.Text.Read as T (decimal)

day6 :: (Integral a) => Int -> Text -> Either String a
day6 steps input = do
    nums <- mapM (readEntire T.decimal) $ T.splitOn "," $ T.dropAround isSpace input
    let state0 = IntMap.fromListWith (+) $ (, 1) <$> nums
        step state = IntMap.fromListWith (+) $ do
            (k, v) <- IntMap.assocs state
            if k == 0 then [(6, v), (8, v)] else [(k - 1, v)]
    pure $ sum $ IntMap.elems $ iterate step state0 !! steps

day6a :: Text -> Either String Int
day6a = day6 80

day6b :: Text -> Either String Int
day6b = day6 256
