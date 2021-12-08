{-|
Module:         Day8
Description:    <https://adventofcode.com/2021/day/8 Day 8: Seven Segment Search>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day8 (day8a, day8b) where

import Control.Arrow (first)
import Control.Monad (guard)
import Data.Bits ((.&.), (.|.), complement, popCount, setBit)
import Data.Char (ord)
import qualified Data.IntMap as IntMap ((!?), fromListWith)
import Data.List (elemIndex, foldl', partition)
import Data.Text (Text)
import qualified Data.Text as T (breakOnEnd, foldl', length, lines, stripSuffix, words)

day8a :: Text -> Int
day8a input = length $ do
    word <- T.lines input >>= T.words . snd . T.breakOnEnd " | "
    guard $ T.length word `elem` [2, 4, 3, 7]

day8b :: Text -> Maybe Int
day8b input = sum <$> mapM handle (T.lines input)

handle :: Text -> Maybe Int
handle line
  | (Just lhs, rhs) <- first (T.stripSuffix " | ") $ T.breakOnEnd " | " line = do
    let signals = bits <$> T.words lhs
        outputs = bits <$> T.words rhs
        counts = IntMap.fromListWith (<>) [(popCount s, [s]) | s <- signals]
    [one] <- counts IntMap.!? 2
    [seven] <- counts IntMap.!? 3
    [four] <- counts IntMap.!? 4
    ([two], threeFive) <- partition ((== 2) . popCountWithout (four .|. seven)) <$> counts IntMap.!? 5
    ([three], [five]) <- pure $ partition ((== 1) . popCountWithout two) threeFive
    ([six], zeroNine) <- partition ((/= 0) . (one .&.) . complement) <$> counts IntMap.!? 6
    ([zero], [nine]) <- pure $ partition ((== 2) . popCountWithout three) zeroNine
    [eight] <- counts IntMap.!? 7
    let digits = [zero, one, two, three, four, five, six, seven, eight, nine]
    foldl' (\x y -> 10 * x + y) 0 <$> mapM (`elemIndex` digits) outputs
  | otherwise = Nothing
  where
    bits = T.foldl' (\acc c -> acc `setBit` (ord c .&. 31)) (0 :: Int)
    popCountWithout antimask = popCount . (.&. complement antimask)
