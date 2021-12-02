{-|
Module:         Day2
Description:    <https://adventofcode.com/2021/day/2 Day 2: Dive!>
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Day2 (day2a, day2b) where

import Common (readEntire)
import Control.Arrow (first)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T (lines, stripPrefix)
import qualified Data.Text.Read as T (decimal)
import Data.Text.Read (Reader)

data Move a = Horizontal a | Vertical a

move :: (Integral a) => Reader (Move a)
move (T.stripPrefix "forward " -> Just f) = first Horizontal <$> T.decimal f
move (T.stripPrefix "down " -> Just d) = first Vertical <$> T.decimal d
move (T.stripPrefix "up " -> Just u) = first (Vertical . negate) <$> T.decimal u
move _ = Left "no parse"

day2a :: Text -> Either String Int
day2a input = do
    moves <- mapM (readEntire move) $ T.lines input
    let x = sum [d | Horizontal d <- moves]
        y = sum [d | Vertical d <- moves]
    pure $ x * y

day2b :: Text -> Either String Int
day2b input = do
    moves <- mapM (readEntire move) $ T.lines input
    let (x, y, _) = foldl' step (0, 0, 0) moves
        step (x0, y0, z) (Horizontal d) = (x0 + d, y0 + d * z, z)
        step (x0, y0, z) (Vertical d) = (x0, y0, z + d)
    pure $ x * y
