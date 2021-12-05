{-|
Module:         Day5
Description:    <https://adventofcode.com/2021/day/5 Day 5: Hydrothermal Venture>
-}
{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, TupleSections, TypeApplications, TypeFamilies #-}
module Day5 (day5a, day5b) where

import qualified Data.Map as Map (elems, fromListWith)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, eof, parse, sepEndBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Num a, MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m [((a, a), (a, a))]
parser = line `sepEndBy` newline <* eof where
    line = (,) <$> pair <* string " -> " <*> pair
    pair = (,) <$> decimal <* char ',' <*> decimal

day5a :: Text -> Either (ParseErrorBundle Text Void) Int
day5a input = do
    segments <- parse (parser @Int) "" input
    pure $ length $ filter (> 1) $ Map.elems $ Map.fromListWith (+) $ do
        ((x0, y0), (x1, y1)) <- segments
        (, 1 :: Int) <$> if
          | x0 == x1 -> [(x0, y) | y <- [min y0 y1..max y0 y1]]
          | y0 == y1 -> [(x, y0) | x <- [min x0 x1..max x0 x1]]
          | otherwise -> []

day5b :: Text -> Either (ParseErrorBundle Text Void) Int
day5b input = do
    segments <- parse (parser @Int) "" input
    pure $ length $ filter (> 1) $ Map.elems $ Map.fromListWith (+) $ do
        ((x0, y0), (x1, y1)) <- segments
        (, 1 :: Int) <$> if
          | x0 == x1 -> [(x0, y) | y <- [min y0 y1..max y0 y1]]
          | y0 == y1 -> [(x, y0) | x <- [min x0 x1..max x0 x1]]
          | x1 - x0 == y1 - y0 -> zip [min x0 x1..max x0 x1] [min y0 y1..max y0 y1]
          | x1 - x0 == y0 - y1 -> zip [min x0 x1..max x0 x1] $ reverse [min y0 y1..max y0 y1]
          | otherwise -> error $ show ((x0, y0), (x1, y1))
