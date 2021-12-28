{-|
Module:         Day23
Description:    <https://adventofcode.com/2021/day/23 Day 23: Amphipod>
-}
{-# LANGUAGE FlexibleContexts, NamedFieldPuns, OverloadedStrings, TupleSections, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day23 (day23a, day23b) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad (guard)
import Data.Array.IArray (Array, IArray, (!), (//), assocs, listArray)
import Data.Char (ord)
import Data.Function (on)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (partition, transpose)
import Data.Map (Map)
import qualified Data.Map as Map ((!?), empty, insert)
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Void (Void)
import qualified Data.Text as T (lines, unlines)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, between, count, count', manyTill, parse, single, skipManyTill)
import Text.Megaparsec.Char (letterChar, newline)

data Cell a = Cell { cellDepth :: Int, cellStack :: [a] } deriving (Eq, Ord)

parser :: (MonadParsec e s m, Token s ~ Char) => m [Cell Char]
parser = do
    _ <- count 2 $ single '#'
    width <- length <$> manyTill (single '#') newline
    _ <- between (single '#') (single '#' *> newline) $ count width (single '.')
    let c = single ' ' <|> single '#'
        go = do
            row <- between c (skipManyTill c newline) line
            if all isNothing row then pure [] else (row :) <$> go
        line = count' 0 width $ Nothing <$ c <|> Just <$> letterChar
        cell cellStack = Cell { cellDepth = 1, cellStack }
    map (cell . catMaybes) . transpose <$> go

day23a :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day23a input = do
    state <- parse parser "" input
    let goals = IntMap.fromDistinctAscList $
            zip [i | (i, Cell { cellStack = _:_}) <- zip [0..] state] ['A'..]
        state' = listArray @Array (0, length state - 1) state
    Right $ solve weight goals Map.empty $ Heap.singleton (0, state')
  where weight = (10 ^) . (subtract `on` ord) 'A'

day23b :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day23b input = day23a $ T.unlines $ pre ++ "  #D#C#B#A#" : "  #D#B#A#C#" : post where
    (pre, post) = splitAt 3 $ T.lines input

solve :: (Alternative f, IArray arr (Cell a), Ord (arr Int (Cell a)), Eq a) => (a -> Int) -> IntMap a -> Map (arr Int (Cell a)) Int -> MinPrioHeap Int (arr Int (Cell a)) -> f Int
solve weight goals = solve' where
    solve' visited (Heap.view -> Just ((_, state), heap))
      | isDone state = pure cost <|> solve' visited'' heap'
      | otherwise = solve' visited'' heap'
      where
        (cost, visited') = maybe (0, Map.insert state 0 visited) (, visited) $ visited Map.!? state
        allMoves = do
            (i, src@Cell { cellStack = a : srcStack }) <- assocs state
            let canLeave b = any (/= b) $ cellStack src
            guard $ maybe True canLeave $ goals IntMap.!? i
            (j, dst) <- assocs state
            let canPass k = cellDepth (state ! k) > 0
                canEnter b = a == b && all (== a) (cellStack dst)
            guard $ i /= j && all canPass (if i < j then [i + 1..j] else [j..i - 1]) &&
                ((||) `on` (`IntMap.member` goals)) i j && maybe True canEnter (goals IntMap.!? j)
            let distance = abs (i - j) + cellDepth src + cellDepth dst - 1
                cost' = cost + distance * weight a
                src' = src { cellDepth = cellDepth src + 1, cellStack = srcStack }
                dst' = dst { cellDepth = cellDepth dst - 1, cellStack = a : cellStack dst }
                state' = state // [(i, src'), (j, dst')]
            guard $ maybe True (> cost') $ visited' Map.!? state'
            pure (j `IntMap.member` goals, (cost', state'))
        (priorityMoves, otherMoves) = partition fst allMoves
        moves
          | (_, priorityMove):_ <- priorityMoves = [priorityMove]
          | otherwise = snd <$> otherMoves
        isBetter (cost', state') = maybe True (> cost') $ visited' Map.!? state'
        moves' = filter isBetter moves
        visited'' = foldr (uncurry Map.insert . swap) visited' moves'
        heap' = foldr Heap.insert heap moves'
    solve' _ _ = empty
    isDone = all isDone' . assocs where
        isDone' (i, Cell { cellDepth, cellStack }) =
            maybe (null cellStack) isDone'' $ goals IntMap.!? i where
            isDone'' a = cellDepth == 1 && all (== a) cellStack
