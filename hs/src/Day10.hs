{-|
Module:         Day10
Description:    <https://adventofcode.com/2021/day/10 Day 10: Syntax Scoring>
-}
module Day10 (day10a, day10b) where

import Control.Monad ((<=<), foldM)
import Data.Either (lefts)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)

day10a :: Text -> Int
day10a input = sum $ lefts $ points . T.unpack <$> T.lines input where
    points ('(':xs) = points xs >>= score ')'
    points ('[':xs) = points xs >>= score ']'
    points ('{':xs) = points xs >>= score '}'
    points ('<':xs) = points xs >>= score '>'
    points xs = Right xs
    score c (x:xs) | c == x = points xs
    score _ (')':_) = Left 3
    score _ (']':_) = Left 57
    score _ ('}':_) = Left 1197
    score _ ('>':_) = Left 25137
    score _ xs = Right xs

day10b :: Text -> Maybe Int
day10b input = median $ mapMaybe (foldM score 0 <=< points "") $ T.unpack <$> T.lines input where
    points cs ('(':xs) = points (')':cs) xs
    points cs ('[':xs) = points (']':cs) xs
    points cs ('{':xs) = points ('}':cs) xs
    points cs ('<':xs) = points ('>':cs) xs
    points (c:cs) (x:xs) | c == x = points cs xs
    points cs [] = Just cs
    points _ _ = Nothing
    score x ')' = Just $ 5 * x + 1
    score x ']' = Just $ 5 * x + 2
    score x '}' = Just $ 5 * x + 3
    score x '>' = Just $ 5 * x + 4
    score _ _ = Nothing
    median [] = Nothing
    median xs = Just $ sort xs !! (length xs `div` 2)
