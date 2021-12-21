{-|
Module:         Day21
Description:    <https://adventofcode.com/2021/day/21 Day 21: Dirac Dice>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Day21 (day21a, day21b) where

import Control.Monad (replicateM)
import Data.Array ((!), listArray, range)
import qualified Data.IntMap as IntMap (assocs, fromListWith)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, between, chunk, parse)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char) => m (Int, Int)
parser = (,) <$>
    between (chunk "Player 1 starting position: ") newline decimal <*>
    between (chunk "Player 2 starting position: ") newline decimal

day21a :: Text -> Either (ParseErrorBundle Text Void) Int
day21a input = do
    (p1, p2) <- parse parser "" input
    pure $ head
      [ n * score1
      | (n, (_, _, score1, score2, _)) <- zip [0, 3..] $ iterate step (p1, p2, 0, 0, 1)
      , score2 >= 1000
      ]
  where
      step (p1, p2, s1, s2, d) = (p2, p1', s2, s1 + p1', (d + 2) `mod` 100 + 1) where
          p1' = (p1 + d + (d `mod` 100 + 1) + ((d + 1) `mod` 100 + 1) - 1) `mod` 10 + 1

day21b :: Text -> Either (ParseErrorBundle Text Void) Int
day21b input = do
    (p1, p2) <- parse parser "" input
    pure $ uncurry max $ scores ! (p1, p2, 0 :: Int, 0 :: Int)
  where
    scores = listArray ((1, 1, 0, 0), (10, 10, 20, 20))
      [ foldr add2 (0, 0)
          [ if s1 + k >= 21 then (n, 0) else (y * n, x * n)
          | (d, n) <- IntMap.assocs $ IntMap.fromListWith (+) [(d, 1) | d <- sum <$> replicateM 3 [1..3]]
          , let k = (p1 + d - 1) `mod` 10 + 1
                (x, y) = scores ! (p2, (p1 + d - 1) `mod` 10 + 1, s2, s1 + k)
          ]
      | (p1, p2, s1, s2) <- range ((1, 1, 0, 0), (10, 10, 20, 20))
      ]
    add2 (a, b) (c, d) = (a + c, b + d)
