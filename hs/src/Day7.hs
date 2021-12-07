{-|
Module:         Day7
Description:    <https://adventofcode.com/2721/day/7 Day 7: The Treachery of Whales>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day7 (day7a, day7b) where

import Common (readEntire)
import Data.List (sort)
import Data.Ratio ((%))
import Data.Text (Text)
import qualified Data.Text as T (lines, splitOn)
import qualified Data.Text.Read as T (decimal)

day7a :: Text -> Either String Int
day7a input = do
    nums <- mapM (readEntire T.decimal) $ T.splitOn "," =<< T.lines input
    let median = sort nums !! (length nums `div` 2)
    pure $ sum [abs $ num - median | num <- nums]

day7b :: Text -> Either String Int
day7b input = do
    nums <- mapM (readEntire T.decimal) $ T.splitOn "," =<< T.lines input
    let mean = sum nums % length nums
        weight mid = sum [dx * (dx + 1) `div` 2 | num <- nums, let dx = abs $ num - mid]
    pure $ min (weight $ floor mean) (weight $ ceiling mean)
