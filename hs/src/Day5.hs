{-|
Module:         Day5
Description:    <https://adventofcode.com/2021/day/5 Day 5: Hydrothermal Venture>
-}
{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Day5 (day5a, day5b) where

import Control.Monad (when)
import Data.Bits (Bits((.&.), (.|.), bitSizeMaybe, shiftL, shiftR, xor), finiteBitSize)
import Data.Function (on)
import qualified Data.IntMap.Strict as IntMap (elems, fromSet, unionsWith)
import qualified Data.IntSet as IntSet (fromDistinctAscList)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Set as Set (empty)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ErrorItem(Label), MonadParsec, ParseErrorBundle, Token, Tokens, eof, failure, parse, sepEndBy)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: forall e s m a. (MonadParsec e s m, Token s ~ Char, IsString (Tokens s), Bits a, Num a, Ord a) => Int -> m [(a, a)]
parser bitsPerComponent
  | bitsPerComponent <= 0 = error "out of range"
  | Just bitSize <- bitSizeMaybe (undefined :: a)
  , bitSize < 2 * bitsPerComponent = error "unsupported"
  | otherwise = line `sepEndBy` newline <* eof
  where
    line = do
        first <- pair <* string " -> "
        second <- pair
        pure (min first second, max first second)
    pair = do
        x <- boundedDecimal <* char ','
        y <- boundedDecimal
        pure $ x `shiftL` bitsPerComponent .|. y
    boundedDecimal = do
        value <- decimal
        when (value `shiftR` bitsPerComponent /= 0) $
            failure (Label <$> nonEmpty "out of range") Set.empty
        pure value

day5a :: Text -> Either (ParseErrorBundle Text Void) Int
day5a input = do
    segments <- parse (parser bitsPerComponent) "" input
    pure $ length $ filter (> 1) $ IntMap.elems $ IntMap.unionsWith (+) $ do
        (start, end) <- segments
        let delta = start `xor` end
        IntMap.fromSet (const (1 :: Int)) . IntSet.fromDistinctAscList <$> if
          | delta `shiftR` bitsPerComponent == 0  -> pure [start..end]
          | delta .&. (1 `shiftL` bitsPerComponent - 1) == 0 ->
                pure [start, start + 1 `shiftL` bitsPerComponent..end]
          | otherwise -> mempty
  where bitsPerComponent = finiteBitSize (0 :: Int) `div` 2

day5b :: Text -> Either (ParseErrorBundle Text Void) Int
day5b input = do
    segments <- parse (parser bitsPerComponent) "" input
    pure $ length $ filter (> 1) $ IntMap.elems $ IntMap.unionsWith (+) $ do
        (start, end) <- segments
        let delta = start `xor` end
        pure $ IntMap.fromSet (const (1 :: Int)) $ IntSet.fromDistinctAscList $ if
          | delta `shiftR` bitsPerComponent == 0  -> [start..end]
          | delta .&. (1 `shiftL` bitsPerComponent - 1) == 0 ->
                [start, start + 1 `shiftL` bitsPerComponent..end]
          | ((-) `on` (`shiftR` bitsPerComponent)) start end ==
                ((-) `on` (.&. (1 `shiftL` bitsPerComponent - 1))) start end ->
                [start, start + 1 `shiftL` bitsPerComponent + 1..end]
          | otherwise -> [start, start + 1 `shiftL` bitsPerComponent - 1..end]
  where bitsPerComponent = finiteBitSize (0 :: Int) `div` 2
