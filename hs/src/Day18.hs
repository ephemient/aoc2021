{-|
Module:         Day18
Description:    <https://adventofcode.com/2021/day/18 Day 18: Snailfish>
-}
{-# LANGUAGE TypeFamilies, ViewPatterns #-}
module Day18 (day18a, day18b) where

import Data.List (inits, tails)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, (<|>), between, eof, parse, sepEndBy, single, try)
import qualified Text.Megaparsec as P (Token)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

data Token a = Open | Close | Value a

parser :: (MonadParsec e s m, P.Token s ~ Char, Num a) => m [Token a]
parser = between (single '[') (single ']') $ do
    lhs <- parser' <* single ','
    rhs <- parser'
    pure $ Open : lhs ++ rhs ++ [Close]
  where parser' = (:[]) . Value <$> try decimal <|> parser

add :: (Integral a, Ord a) => [Token a] -> [Token a] -> [Token a]
add lhs rhs = explode $ Open : lhs ++ rhs ++ [Close] where
    explode tokens = explode' (0 :: Int) [] tokens where
        explode' n pre (Open : Value x : Value y : Close : post) | n >= 4 = explode $
            reverse (modifyFirstValue (+ x) pre) ++ Value 0 : modifyFirstValue (+ y) post
        explode' n pre (cur : post) = explode' n' (cur : pre) post where
            n' | Open <- cur = n + 1 | Close <- cur = n - 1 | otherwise = n
        explode' _ _ [] = split tokens
    modifyFirstValue f (Value x : rest) = Value (f x) : rest
    modifyFirstValue f (x : rest) = x : modifyFirstValue f rest
    modifyFirstValue _ [] = []
    split tokens = split' [] tokens where
        split' pre (Value x : post) | x > 9 = explode $
            reverse pre ++ Open : Value (x `div` 2) : Value ((x + 1) `div` 2) : Close : post
        split' pre (cur : post) = split' (cur : pre) post
        split' _ [] = tokens

magnitude :: (Num a) => [Token a] -> Maybe a
magnitude input
  | (result, []) <- magnitude' input = result
  | otherwise = Nothing
  where
    magnitude' (Open : (magnitude' -> (Just lhs, magnitude' -> (Just rhs, Close : rest)))) =
        (Just $ 3 * lhs + 2 * rhs, rest)
    magnitude' (Value value : rest) = (Just value, rest)
    magnitude' rest = (Nothing, rest)

day18a :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day18a input = do
    list <- parse (parser `sepEndBy` newline <* eof) "" input
    pure $ nonEmpty list >>= magnitude . foldl1 add

day18b :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day18b input = do
    list <- parse (parser `sepEndBy` newline <* eof) "" input
    let sums = do
            (pre, x : post) <- zip (inits list) (tails list)
            y <- pre ++ post
            [add x y, add y x]
    pure $ fmap maximum $ mapM magnitude sums >>= nonEmpty
