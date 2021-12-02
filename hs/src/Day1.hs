{-|
Module:         Day1
Description:    <https://adventofcode.com/2021/day/1 Day 1: Sonar Sweep>
-}
{-# LANGUAGE NondecreasingIndentation, ParallelListComp, TypeApplications #-}
module Day1 (day1a, day1b) where

import Common (readEntire)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal)

day1a :: Text -> Either String Int
day1a input = do
    nums <- mapM (readEntire @Int T.decimal) (T.lines input)
    pure $ length $ filter id $ zipWith (<) nums $ drop 1 nums

day1b :: Text -> Either String Int
day1b input = do
    nums <- mapM (readEntire @Int T.decimal) (T.lines input)
    let sums = [x + y + z | x <- nums | y <- drop 1 nums | z <- drop 2 nums]
    pure $ length $ filter id $ zipWith (<) sums $ drop 1 sums
