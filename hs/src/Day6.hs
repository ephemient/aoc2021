{-|
Module:         Day6
Description:    <https://adventofcode.com/2021/day/6 Day 6: Lanternfish>
-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Day6 (day6a, day6b) where

import Common (readEntire)
import Data.Array.Unboxed (UArray, (!))
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.Read as T (decimal)
import Day6Meta (mkLUT)

day6 :: UArray Int Int -> Text -> Either String Int
day6 lut input = do
    nums <- mapM (readEntire T.decimal) $ T.splitOn "," =<< T.lines input
    pure $ sum [lut ! num | num <- nums]

day6a :: Text -> Either String Int
day6a = day6 [mkLUT| 80 |]

day6b :: Text -> Either String Int
day6b = day6 [mkLUT| 256 |]
