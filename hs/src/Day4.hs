{-|
Module:         Day4
Description:    <https://adventofcode.com/2021/day/4 Day 4: Giant Squid>
-}
{-# LANGUAGE TupleSections, TypeFamilies #-}
module Day4 (day4a, day4b) where

import Control.Arrow (left)
import Control.Monad (foldM_)
import Data.Either (lefts)
import Data.Functor (($>))
import Data.List (transpose)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, eof, parse, sepBy, sepBy1, sepEndBy, sepEndBy1, skipSome)
import Text.Megaparsec.Char (char, hspace, hspace1, newline)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (Num a, MonadParsec e s m, Token s ~ Char) => m ([a], [[[Maybe a]]])
parser = do
    draws <- decimal `sepBy` char ',' <* skipSome newline
    let board = line `sepEndBy1` newline
        line = hspace *> (Just <$> decimal) `sepBy1` hspace1
    boards <- board `sepEndBy` newline
    eof $> (draws, boards)

play :: (Eq a, Num a) => a -> [[Maybe a]] -> Either a [[Maybe a]]
play draw board
  | any (all isNothing) board' = Left score
  | any (all isNothing) $ transpose board' = Left score
  | otherwise = Right board'
  where
    board' = [[if x == Just draw then Nothing else x | x <- line] | line <- board]
    score = draw * sum [x | line <- board', Just x <- line]

day4a :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day4a input = do
    (draws, boards) <- parse parser "" input
    pure $ either Just (const Nothing) $ foldM_ (flip $ mapM . play) boards draws

day4b :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day4b input = do
    (draws, boards) <- parse parser "" input
    let scores = [foldM_ f board $ zip [0 :: Int ..] draws | board <- boards]
        f board (n, draw) = left (n,) $ play draw board
    pure $ snd . maximum <$> nonEmpty (lefts scores)
