{-|
Module:         Day13
Description:    <https://adventofcode.com/2021/day/13 Day 13: Transparent Origami>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, TypeApplications, TypeFamilies #-}
module Day13 (day13a, day13b) where

import Control.Arrow ((&&&), (***))
import Data.Array.Unboxed (UArray, (!), accumArray)
import Data.Bool (bool)
import Data.Functor (($>))
import Data.Semigroup (Max(..), Min(..))
import qualified Data.Set as Set (fromList, size)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), chunk, eof, parse, sepEndBy, sepEndBy1, single)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

parsePair :: (Num a, MonadParsec e s m, Token s ~ Char) => m (a, a)
parsePair = (,) <$> decimal <* single ',' <*> decimal

parseFold :: (Num a, Ord a, MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ((a, a) -> (a, a))
parseFold = chunk "fold along " *> (single 'x' $> foldX <|> single 'y' $> foldY) <*> (single '=' *> decimal)

foldX, foldY :: (Num a, Ord a) => a -> (a, a) -> (a, a)
foldX x' (x, y) = (x' - abs (x - x'), y)
foldY y' (x, y) = (x, y' - abs (y - y'))

day13a :: Text -> Either (ParseErrorBundle Text Void) Int
day13a input = do
    (points, fold) <- parse parser "" input
    pure $ Set.size $ Set.fromList $ fold <$> points
  where parser = (,) <$> parsePair @Int `sepEndBy` newline <* newline <*> parseFold

day13b :: Text -> Either (ParseErrorBundle Text Void) [String]
day13b input = do
    (points, folds) <- parse parser "" input
    let points' = foldr (flip (.)) id folds <$> points
        ((Min x0, Min y0), (Max x1, Max y1)) = mconcat $ ((Min *** Min) &&& (Max *** Max)) <$> points'
        bitmap = accumArray @UArray (||) False ((x0, y0), (x1, y1)) $ (, True) <$> points'
    pure [[bool '\x2591' '\x2593' $ bitmap ! (x, y) | x <- [x0..x1]] | y <- [y0..y1]]
  where parser = (,) <$> parsePair @Int `sepEndBy1` newline <* newline <*> parseFold `sepEndBy` newline <* eof
