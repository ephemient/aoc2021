{-|
Module:         Day9
Description:    <https://adventofcode.com/2021/day/9 Day 9: Smoke Basin>
-}
{-# LANGUAGE TypeApplications #-}
module Day9 (day9a, day9b) where

import Data.Char (digitToInt, isDigit)
import Data.Graph.Inductive (Gr, buildGr, components)
import Data.List (mapAccumL, mapAccumR, sortOn, zipWith5)
import Data.Maybe (catMaybes)
import Data.Ord (Down(Down))
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

day9a :: Text -> Int
day9a input = sum risks where
    heights = T.unpack <$> T.lines input
    risks = concat $ zipWith3 basins heights
        (repeat maxBound : heights) (drop 1 heights ++ [repeat maxBound])
    basins row above below = catMaybes $ zipWith5 basin row above below
        (maxBound : row) (drop 1 row ++ repeat maxBound)
    basin x above below left right
      | isDigit x, x < above, x < below, x < left, x < right = Just $ digitToInt x + 1
      | otherwise = Nothing

day9b :: Text -> Int
day9b input = product $ take 3 $ sortOn Down $ map length $ components basins where
    basins = buildGr @Gr $ concat $ snd $ mapAccumR mkRow (0, []) $ T.unpack <$> T.lines input
    mkRow (n, prev) line = (next, contexts) where
        next@(_, ns) = mapAccumL f n line
        f n' c | isDigit c, digitToInt c < 9 = (n' + 1, Just n')
        f n' _ = (n', Nothing)
        contexts = catMaybes $ zipWith3 g ns (prev ++ repeat Nothing) (drop 1 ns ++ repeat Nothing)
        g (Just n') above' right =
            Just ([((), m) | m <- catMaybes [above', right]], n', (), [])
        g _ _ _ = Nothing
