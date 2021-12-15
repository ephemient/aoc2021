{-|
Module:         Day15
Description:    <https://adventofcode.com/2021/day/15 Day 15: Chiton>
-}
{-# LANGUAGE ViewPatterns #-}
module Day15 (day15a, day15b) where

import Control.Monad (foldM, guard)
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt, isDigit)
import Data.Functor (($>))
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (insert, notMember, singleton)
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Text (Text)
import qualified Data.Text as T (all, length, lines, unpack)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V ((!), fromList, length)
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV (read, replicate, write)

day15 :: Int -> Vector Int -> Maybe Int
day15 width risks = runST $ do
    bests <- MV.replicate (V.length risks) maxBound
    MV.write bests 0 0
    go bests (Heap.singleton (0, 0)) (IntSet.singleton 0)
  where
    f :: MVector s Int -> Int -> (MinPrioHeap Int Int, IntSet) -> (Int, Int) -> ST s (MinPrioHeap Int Int, IntSet)
    f bests c (heap, heapSet) (y, x)
      | x < 0 || x >= width || y < 0 || y * width + x >= V.length risks = pure (heap, heapSet)
      | otherwise = do
        best <- MV.read bests i
        if risk < best && IntSet.notMember i heapSet
          then MV.write bests i risk $> (Heap.insert (risk, i) heap, IntSet.insert i heapSet)
          else pure (heap, heapSet)
      where
        i = y * width + x
        risk = c + risks V.! i
    go :: MVector s Int -> MinPrioHeap Int Int -> IntSet -> ST s (Maybe Int)
    go bests (Heap.view -> Just ((_, i), heap)) heapSet
      | i == V.length risks - 1 = Just <$> MV.read bests i
      | otherwise = do
        c <- MV.read bests i
        (heap', heapSet') <- foldM (f bests c) (heap, heapSet) $
            let (y, x) = i `divMod` width in [(y, x - 1), (y - 1, x), (y + 1, x), (y, x + 1)]
        go bests heap' heapSet'
    go _ _ _ = pure Nothing

day15a :: Text -> Maybe Int
day15a input = do
    input'@(line :| lines') <- nonEmpty $ T.lines input
    let width = T.length line
    guard $ width > 0 && all ((== width) . T.length) lines' && all (T.all isDigit) input'
    day15 width $ V.fromList $ concatMap (fmap digitToInt . T.unpack) input'

day15b :: Text -> Maybe Int
day15b input = do
    input'@(line :| lines') <- nonEmpty $ T.lines input
    let width = T.length line
    guard $ width > 0 && all ((== width) . T.length) lines' && all (T.all isDigit) input'
    day15 (5 * width) $ V.fromList $ do
        i <- [0..4]
        line' <- line:lines'
        j <- [0..4]
        [(digitToInt c - 1 + i + j) `mod` 9 + 1 | c <- T.unpack line']
