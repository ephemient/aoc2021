{-|
Module:         Day13
Description:    <https://adventofcode.com/2021/day/13 Day 13: Transparent Origami>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, TypeApplications, TypeFamilies #-}
module Day13 (day13a, day13b) where

import Control.Arrow ((&&&), (***))
import Data.Array.Unboxed (UArray, accumArray, elems)
import Data.List.Split (chunksOf)
import Data.Semigroup (Max(..), Min(..))
import qualified Data.Set as Set (fromList, size)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), chunk, eof, parse, sepEndBy, sepEndBy1, single)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

parsePair :: (Num a, MonadParsec e s m, Token s ~ Char) => m (a, a)
parsePair = flip (,) <$> decimal <* single ',' <*> decimal

parseFold :: (Num a, Ord a, MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ((a, a) -> (a, a))
parseFold = chunk "fold along " *> (foldX <$ single 'x' <|> foldY <$ single 'y') <*> (single '=' *> decimal)

foldX, foldY :: (Num a, Ord a) => a -> (a, a) -> (a, a)
foldX x' (y, x) = (y, x' - abs (x - x'))
foldY y' (y, x) = (y' - abs (y - y'), x)

day13a :: Text -> Either (ParseErrorBundle Text Void) Int
day13a input = do
    (points, fold) <- parse parser "" input
    pure $ Set.size $ Set.fromList $ fold <$> points
  where parser = (,) <$> parsePair @Int `sepEndBy` newline <* newline <*> parseFold

day13b :: Text -> Either (ParseErrorBundle Text Void) [String]
day13b input = do
    (points, folds) <- parse parser "" input
    let points' = foldr (flip (.)) id folds <$> points
        ((Min y0, Min x0), (Max y1, Max x1)) = mconcat $ ((Min *** Min) &&& (Max *** Max)) <$> points'
        bitmap = accumArray @UArray (const id) '\x2591' ((y0, x0), (y1, x1)) $ (, '\x2593') <$> points'
    pure $ chunksOf (x1 - x0 + 1) $ elems bitmap
  where parser = (,) <$> parsePair @Int `sepEndBy1` newline <* newline <*> parseFold `sepEndBy` newline <* eof
