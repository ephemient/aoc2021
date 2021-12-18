{-|
Module:         Day18
Description:    <https://adventofcode.com/2021/day/18 Day 18: Snailfish>
-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeFamilies #-}
module Day18 (day18a, day18b) where

import Data.List (inits, tails)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromJust)
import qualified Data.Set as Set (empty)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Parsec, Token, (<|>), between, eof, parse, parseMaybe, sepEndBy, single, token, try)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

data Snail a = Snail (Either (Snail a) a) (Either (Snail a) a)
data SnailToken a = Open | Close | Value a deriving (Eq, Ord)

parser :: (MonadParsec e s m, Token s ~ Char, Num a) => m (Snail a)
parser = between (single '[') (single ']') $ Snail <$> parser' <* single ',' <*> parser' where
    parser' = Right <$> try decimal <|> Left <$> parser

snailReduce :: forall a. (Integral a, Ord a) => Snail a -> Snail a
snailReduce = fromJust . parseMaybe (tokenParser <* eof) . explode 0 [] . flatten where
    tokenParser :: Parsec Void [SnailToken a] (Snail a)
    tokenParser = between (single Open) (single Close) $ Snail <$> tokenParser' <*> tokenParser' where
        tokenParser' = Right <$> try value <|> Left <$> tokenParser
        value = flip token Set.empty $ \case
            Value a -> Just a
            _ -> Nothing
    flatten :: Snail a -> [SnailToken a]
    flatten (Snail lhs rhs) = Open : flatten' lhs ++ flatten' rhs ++ [Close] where
        flatten' = either flatten $ (:[]) . Value
    explode :: Int -> [SnailToken a] -> [SnailToken a] -> [SnailToken a]
    explode n pre (Open : Value x : Value y : Close : post) | n >= 4 = explode 0 [] $
        reverse (modifyFirstValue (+ x) pre) ++ Value 0 : modifyFirstValue (+ y) post
    explode n pre (cur : post) = explode n' (cur : pre) post where
        n' | Open <- cur = n + 1 | Close <- cur = n - 1 | otherwise = n
    explode _ pre [] = split [] $ reverse pre
    modifyFirstValue :: (a -> a) -> [SnailToken a] -> [SnailToken a]
    modifyFirstValue f (Value x : rest) = Value (f x) : rest
    modifyFirstValue f (x : rest) = x : modifyFirstValue f rest
    modifyFirstValue _ [] = []
    split :: [SnailToken a] -> [SnailToken a] -> [SnailToken a]
    split pre (Value x : post) | x > 9 = explode 0 [] $
        reverse pre ++ Open : Value (x `div` 2) : Value ((x + 1) `div` 2) : Close : post
    split pre (cur : post) = split (cur : pre) post
    split k [] = reverse k

snailAdd :: (Integral a, Ord a) => Snail a -> Snail a -> Snail a
snailAdd lhs rhs = snailReduce $ Snail (Left lhs) (Left rhs)

magnitude :: (Num a) => Snail a -> a
magnitude (Snail lhs rhs) = 3 * magnitude' lhs + 2 * magnitude' rhs where
    magnitude' = either magnitude id

day18a :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day18a input = do
    snails <- parse (parser `sepEndBy` newline <* eof) "" input
    pure $ magnitude . foldl1 snailAdd <$> nonEmpty snails

day18b :: Text -> Either (ParseErrorBundle Text Void) (Maybe Int)
day18b input = do
    snails <- parse (parser `sepEndBy` newline <* eof) "" input
    pure $ fmap maximum $ nonEmpty $ do
        (pre, x : post) <- zip (inits snails) (tails snails)
        y <- pre ++ post
        magnitude <$> [snailAdd x y, snailAdd y x]
