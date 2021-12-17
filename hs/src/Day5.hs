{-|
Module:         Day5
Description:    <https://adventofcode.com/2021/day/5 Day 5: Hydrothermal Venture>
-}
{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, TypeApplications, TypeFamilies #-}
module Day5 (day5a, day5b) where

import Control.Monad (guard)
import Data.Function (on)
import Data.List (tails)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, size)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, eof, parse, sepEndBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Num a, Ord a, MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m [((a, a), (a, a))]
parser = line `sepEndBy` newline <* eof where
    line = do
        d0 <- pair <* string " -> "
        d1 <- pair
        pure $ if d0 < d1 then (d0, d1) else (d1, d0)
    pair = (,) <$> decimal <* char ',' <*> decimal

intersections :: (Enum a, Integral a, Ord a, Show a) => [((a, a), (a, a))] -> Set (a, a)
intersections segments = Set.fromList $ do
    ((x00, y00), (x01, y01)):segments' <- tails segments
    ((x10, y10), (x11, y11)) <- segments'
    let m0 = (y01 - y00) `div` (x01 - x00)
        m1 = (y11 - y10) `div` (x11 - x10)
        a0 = y00 - m0 * x00
        a1 = y10 - m1 * x10
    if
      | x00 == x01 && x10 == x11 -> guard (x00 == x10) *>
            [(x00, y) | y <- [max y00 y10..min y01 y11]]
      | x00 == x01 -> guard (x10 <= x00 && x00 <= x11) *>
            let y = (y11 - y10) `div` (x11 - x10) * (x00 - x10) + y10
            in (x00, y) <$ guard (y00 <= y && y <= y01)
      | x10 == x11 -> guard (x00 <= x10 && x10 <= x01) *>
            let y = (y01 - y00) `div` (x01 - x00) * (x10 - x00) + y00
            in (x10, y) <$ guard (y10 <= y && y <= y11)
      | m0 == m1 -> guard (a0 == a1) *>
            [(x, m0 * x + a0) | x <- [max x00 x10..min x01 x11]]
      | (x, 0) <- (a0 - a1) `divMod` (m1 - m0)
      , max x00 x10 <= x && x <= min x01 x11 ->
            [(x, m0 * x + a0)]
      | otherwise -> mempty

day5a :: Text -> Either (ParseErrorBundle Text Void) Int
day5a input = do
    segments <- parse (parser @Int) "" input
    pure $ Set.size $ intersections $
        filter ((||) <$> uncurry ((==) `on` fst) <*> uncurry ((==) `on` snd)) segments

day5b :: Text -> Either (ParseErrorBundle Text Void) Int
day5b input = do
    segments <- parse (parser @Int) "" input
    pure $ Set.size $ intersections segments
