{-|
Module:         Day8
Description:    <https://adventofcode.com/2021/day/8 Day 8: Seven Segment Search>
-}
{-# LANGUAGE OverloadedStrings #-}
module Day8 (day8a, day8b) where

import Control.Arrow (first)
import Control.Monad (guard)
import Data.Char (chr, ord)
import Data.List (elemIndex, foldl', permutations, sort)
import Data.Map (Map)
import qualified Data.Map as Map ((!?), fromList)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T (breakOnEnd, length, lines, unpack, stripSuffix, words)

day8a :: Text -> Int
day8a input = length $ do
    word <- T.lines input >>= T.words . snd . T.breakOnEnd " | "
    guard $ T.length word `elem` [2, 4, 3, 7]

day8b :: Text -> Maybe Int
day8b input = sum <$> mapM handle (T.lines input)

forms :: Map String Int
forms = Map.fromList $ zip
    ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]
    [0..9]

handle :: Text -> Maybe Int
handle line
  | (Just lhs, rhs) <- first (T.stripSuffix " | ") $ T.breakOnEnd " | " line = do
    let canon = sort . T.unpack
        signals = canon <$> T.words lhs
        outputs = canon <$> T.words rhs
    indices <- mapM (`elemIndex` signals) outputs
    order <- listToMaybe $ do
        derange <- permutations [0..6]
        let mapping = map (chr . (+ ord 'a')) . sort . map ((derange !!) . subtract (ord 'a') . ord)
            forms' = mapping <$> signals
        maybeToList $ mapM (forms Map.!?) forms'
    pure $ foldl' (\x y -> 10 * x + y) 0 $ (order !!) <$> indices
  | otherwise = Nothing
