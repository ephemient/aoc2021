{-|
Module:         Day4
Description:    <https://adventofcode.com/2021/day/4 Day 4: Giant Squid>
-}
{-# LANGUAGE NamedFieldPuns, TupleSections, TypeFamilies #-}
module Day4 (day4a, day4b) where

import Control.Arrow (left)
import Control.Monad (foldM_)
import Data.Either (lefts)
import Data.Functor (($>))
import Data.List.NonEmpty (nonEmpty)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap (assocs, fromListWith, updateLookupWithKey)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (empty, fromDistinctAscList, isSubsetOf, singleton, size, union)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, eof, parse, sepBy, sepBy1, sepEndBy, sepEndBy1, skipSome)
import Text.Megaparsec.Char (char, hspace, hspace1, newline)
import Text.Megaparsec.Char.Lexer (decimal)

data BingoBoard = BingoBoard
  { range :: ((Int, Int), (Int, Int))
  , unmarked :: IntMap IntSet
  , marked :: IntSet
  }

parser :: (MonadParsec e s m, Token s ~ Char) => m ([Int], [BingoBoard])
parser = do
    draws <- decimal `sepBy` char ',' <* skipSome newline
    boards <- board `sepEndBy` newline
    eof $> (draws, boards)
  where
    board = convert <$> (hspace *> decimal `sepBy1` hspace1) `sepEndBy1` newline
    convert rows = BingoBoard { range = ((0, 0), (w - 1, h - 1)), unmarked, marked = IntSet.empty } where
        w = maximum $ length <$> rows
        h = length rows
        unmarked = IntMap.fromListWith IntSet.union
            [(n, IntSet.singleton i) | (y, ns) <- zip [0..] rows, (i, n) <- zip [y, y + w..] ns]

play :: Int -> BingoBoard -> Either Int BingoBoard
play draw board = case IntMap.updateLookupWithKey (const $ const Nothing) draw $ unmarked board of
    (Nothing, _) -> Right board
    (Just marks, unmarked') ->
        let board' = board
              { unmarked = unmarked'
              , marked = IntSet.union marks $ marked board
              }
        in if isBingo board'
           then Left $ draw * sum [n * IntSet.size i | (n, i) <- IntMap.assocs unmarked']
           else Right board'

isBingo :: BingoBoard -> Bool
isBingo BingoBoard { range = ((x0, y0), (x1, y1)), marked } =
    any (flip IntSet.isSubsetOf marked . IntSet.fromDistinctAscList) $
        [[start..start + y1 - y0] | start <- [0, y1 - y0 + 1..(y1 - y0 + 1) * (x1 - x0)]] ++
        [[start, start + y1 - y0 + 1..start + (y1 - y0 + 1) * (x1 - x0)] | start <- [0..y1 - y0]]

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
