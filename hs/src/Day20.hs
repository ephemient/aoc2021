{-|
Module:         Day20
Description:    <https://adventofcode.com/2021/day/20 Day 20: Trench Map>
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Day20 (day20a, day20b) where

import Data.Bits ((.&.))
import Data.Char (ord)
import Data.List (tails, unfoldr)
import Data.Text (Text)
import qualified Data.Text as T (compareLength, concat, cons, count, foldl', index, length, lines, pack, null, replicate, singleton, snoc, tails, take)

day20 :: Text -> Maybe [Int]
day20 (T.lines -> alg:blank:image0)
  | T.length alg == 512 && T.null blank
  = Just $ sum (T.count "#" <$> image0) : unfoldr enhance (image0, '.') ++ repeat 0
  where
    get s = T.index alg $ T.foldl' (\x y -> 2 * x + ord y .&. 1) 0 s
    enhance (image@(line0:_), fill) =
        Just (sum $ T.count "#" <$> image1, (image1, get $ T.replicate 9 $ T.singleton fill)) where
        line4 = T.replicate (T.length line0) $ T.singleton fill
        win3 = map (T.take 3) . takeWhile ((> LT) . flip T.compareLength 3) . T.tails .
            T.cons fill . T.cons fill . flip T.snoc fill . flip T.snoc fill
        image1 =
          [ T.pack [get $ T.concat [a, b, c] | (a, b, c) <- zip3 (win3 line1) (win3 line2) (win3 line3)]
          | line1:line2:line3:_ <- tails $ line4 : line4 : image ++ [line4, line4]
          ]
    enhance _ = Nothing
day20 _ = Nothing

day20a :: Text -> Maybe Int
day20a = fmap (!! 2) . day20

day20b :: Text -> Maybe Int
day20b = fmap (!! 50) . day20
