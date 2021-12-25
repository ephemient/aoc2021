{-|
Module:         Day25
Description:    <https://adventofcode.com/2021/day/25 Day 25: Sea Cucumber>
-}
{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Day25 (day25) where

import Control.Arrow (first, second)
import Control.Monad (guard)
import Data.Array.Unboxed (UArray, (!), (//), accumArray, assocs)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T (length, lines, unpack)

day25 :: Text -> Maybe Int
day25 input = do
    let inputs = T.lines input
        height = length inputs
    width <- maximum $ Nothing : map (Just . T.length) inputs
    let state0 = accumArray @UArray (const id) '.' ((0, 0), (height - 1, width - 1))
            [((y, x), c) | (y, line) <- zip [0..] inputs, (x, c) <- zip [0..] $ T.unpack line]
        step state = state'' <$ guard (a || b) where
            (a, state') = step' (second $ \x -> succ x `mod` width) '>' state
            (b, state'') = step' (first $ \y -> succ y `mod` height) 'v' state'
        step' f d state = (not $ null acc, state // acc) where
            acc = do
                (i, c) <- assocs state
                guard $ c == d && state ! f i == '.'
                [(i, '.'), (f i, d)]
    pure $ length $ takeWhile isJust $ iterate (>>= step) $ Just state0
