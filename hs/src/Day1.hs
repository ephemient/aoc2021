{-|
Module:         Day1
Description:    <https://adventofcode.com/2021/day/1 Day 1: Sonar Sweep>
-}
{-# LANGUAGE NondecreasingIndentation, ParallelListComp, TypeApplications #-}
module Day1 (day1a, day1b) where

import Common (readEntire)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE (tail)
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.Read as T (decimal)

day1a :: Text -> Either String Int
day1a input = do
    nums0 <- mapM (readEntire @Int T.decimal) (T.lines input)
    nums1 <- maybe (Left "too few inputs") Right $ NE.tail <$> nonEmpty nums0
    pure $ length $ filter id $ zipWith (<) nums0 nums1

day1b :: Text -> Either String Int
day1b input = do
    nums0 <- mapM (readEntire @Int T.decimal) (T.lines input)
    maybe (Left "too few inputs") Right $ do
    nums1 <- NE.tail <$> nonEmpty nums0
    nums2 <- NE.tail <$> nonEmpty nums1
    let sums0 = [x + y + z | x <- nums0 | y <- nums1 | z <- nums2]
    sums1 <- NE.tail <$> nonEmpty sums0
    pure $ length $ filter id $ zipWith (<) sums0 sums1
