{-|
Module:         Day16
Description:    <https://adventofcode.com/2021/day/16 Day 16: Packet Decoder>
-}
{-# LANGUAGE FlexibleContexts, LambdaCase, NamedFieldPuns, RecordWildCards, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day16 (day16a, day16b) where

import Control.Monad.State (MonadState, evalStateT, gets, put, state)
import Data.Bits (testBit)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, count, getOffset, parse)
import Text.Megaparsec.Char (hexDigitChar)

data Packet a
  = Literal { version :: Int, value :: a }
  | Operator { version :: Int, tag :: Int, packets :: [Packet a] }

parser :: (Num a, MonadParsec e s m, Token s ~ Char) => m (Packet a)
parser = evalStateT packet [] where
    packet :: (Num a, MonadState [Bool] m, MonadParsec e s m, Token s ~ Char) => m (Packet a)
    packet = do
        version <- bits 3
        tag <- bits 3
        if tag == 4 then Literal version <$> literal else do
            b <- bit
            packets <- if b then bits 11 >>= flip count packet else bits 15 >>= parseSpan
            pure Operator { .. }
    getHead :: (MonadState [a] m) => m (Maybe a)
    getHead = state $ \case
        (x:xs) -> (Just x, xs)
        xs -> (Nothing, xs)
    bit :: (MonadState [Bool] m, MonadParsec e s m, Token s ~ Char) => m Bool
    bit = getHead >>= maybe nextBit pure where
        nextBit = do
            n <- digitToInt <$> hexDigitChar
            testBit n 3 <$ put (testBit n <$> [2, 1, 0])
    bits :: (Num a, MonadState [Bool] m, MonadParsec e s m, Token s ~ Char) => Int -> m a
    bits n = foldl' f 0 <$> count n bit where f acc b = 2 * acc + if b then 1 else 0
    literal :: (Num a, MonadState [Bool] m, MonadParsec e s m, Token s ~ Char) => m a
    literal = literal' 0 where
        literal' n = do
            b <- bit
            n' <- f n <$> bits 4
            if b then literal' n' else pure n'
        f acc n = 16 * acc + n
    getOffsetBits :: (MonadState [a] m, MonadParsec e s m) => m Int
    getOffsetBits = (-) . (* 4) <$> getOffset <*> gets length
    parseSpan :: (Num a, MonadState [Bool] m, MonadParsec e s m, Token s ~ Char) => Int -> m [Packet a]
    parseSpan 0 = pure []
    parseSpan n = do
        offset0 <- getOffsetBits
        p <- packet
        offset1 <- getOffsetBits
        (:) p <$> parseSpan (n - (offset1 - offset0))

day16a :: Text -> Either (ParseErrorBundle Text Void) Int
day16a input = do
    packet <- parse (parser @Int) "" input
    pure $ sumVersions packet
  where
    sumVersions Literal { version } = version
    sumVersions Operator { version, packets } = version + sum (sumVersions <$> packets)

day16b :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day16b input = do
    packet <- parse parser "" input
    pure $ eval packet
  where
    eval Literal { value } = Just value
    eval Operator { tag = 0, packets } = sum <$> mapM eval packets
    eval Operator { tag = 1, packets } = product <$> mapM eval packets
    eval Operator { tag = 2, packets } = minimum <$> mapM eval packets
    eval Operator { tag = 3, packets } = maximum <$> mapM eval packets
    eval Operator { tag = 5, packets = [eval -> Just lhs, eval -> Just rhs] } =
        Just $ if lhs > rhs then 1 else 0
    eval Operator { tag = 6, packets = [eval -> Just lhs, eval -> Just rhs] } =
        Just $ if lhs < rhs then 1 else 0
    eval Operator { tag = 7, packets = [eval -> Just lhs, eval -> Just rhs] } =
        Just $ if lhs == rhs then 1 else 0
    eval _ = Nothing
