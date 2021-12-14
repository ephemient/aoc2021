{-|
Module:         Day14
Description:    <https://adventofcode.com/21421/day/14 Day 14: Extended Polymerization>
-}
{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Day14 (day14a, day14b) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import qualified Data.Map as Map ((!), elems, fromList, fromListWith, size, toList)
import Data.Semigroup (Max(..), Min(..))
import qualified Data.Set as Set (fromList, size)
import Data.Text (Text)
import qualified Data.Text as T (head, last, length, lines, null, stripPrefix, tail, uncons, unpack, zip)

day14 :: Text -> Maybe [Int]
day14 input = do
    initial : e : rest <- pure $ T.lines input
    guard $ T.null e && T.length initial >= 2
    rules <- Map.fromList <$> mapM parseRule rest
    let nChars = Set.size $ Set.fromList $ T.unpack initial ++ do
            ((x, y), z) <- Map.toList rules
            x : y : concatMap (\(a, b) -> [a, b]) z
    guard $ Map.size rules == nChars * nChars
    let state0 = Map.fromListWith (+) $ (, 1) <$> T.zip initial (T.tail initial)
        step state = Map.fromListWith (+)
          [ (dst, n)
          | (src, n) <- Map.toList state
          , dst <- rules Map.! src
          ]
        extract state = (hi - lo) `div` 2 where
            (Min lo, Max hi) = mconcat $ map (Min &&& Max) $ Map.elems $ Map.fromListWith (+) $
                (T.head initial, 1) : (T.last initial, 1) :
                    [(c, n) | ((a, b), n) <- Map.toList state, c <- [a, b]]
    pure $ extract <$> iterate step state0
  where
    parseRule line = do
        (x, line') <- T.uncons line
        (y, line'') <- T.uncons line'
        (z, line''') <- T.uncons =<< T.stripPrefix " -> " line''
        guard $ T.null line'''
        pure ((x, y), [(x, z), (z, y)])

day14a :: Text -> Maybe Int
day14a = fmap (!! 10) . day14

day14b :: Text -> Maybe Int
day14b = fmap (!! 40) . day14
