{-|
Module:         Day3
Description:    <https://adventofcode.com/2021/day/3 Day 3: Binary Diagnostic>
-}
module Day3 (day3a, day3b) where

import Data.Bits (xor)
import Data.Char (chr, ord)
import Data.List (group, maximumBy, minimumBy, sort, transpose)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T (lines, unpack)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle, eof, parse)
import Text.Megaparsec.Char.Lexer (binary)

common, uncommon :: (Ord a) => [a] -> a
common xs = head $ maximumBy (comparing length) $ group $ sort xs
uncommon xs = head $ minimumBy (comparing length) $ group $ sort xs

day3a :: Text -> Either (ParseErrorBundle String Void) Int
day3a input = (*) <$> parse (binary <* eof) "" gamma <*> parse (binary <* eof) "" epsilon where
    nums = T.unpack <$> T.lines input
    gamma = common <$> transpose nums
    epsilon = chr . xor 1 . ord <$> gamma

day3b :: Text -> Either (ParseErrorBundle String Void) Int
day3b input = (*) <$> parse (binary <* eof) "" o2 <*> parse (binary <* eof) "" co2 where
    nums = T.unpack <$> T.lines input
    scrub _ xs | null xs || any null xs = []
    scrub f xs = x : scrub f [ys | y:ys <- xs, y == x] where x = f $ head <$> xs
    o2 = scrub common nums
    co2 = scrub uncommon nums
