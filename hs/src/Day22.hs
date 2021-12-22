{-|
Module:         Day22
Description:    <https://adventofcode.com/2021/day/22 Day 22: Reactor Reboot>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day22 (day22a, day22b) where

import Data.Ix (Ix, inRange, rangeSize)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), chunk, eof, parse, sepEndBy)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m (Bool, ((a, a, a), (a, a, a)))
parser = do
    b <- (True <$ chunk "on") <|> (False <$ chunk "off")
    x0 <- chunk " x=" *> signed (pure ()) decimal
    x1 <- chunk ".." *> signed (pure ()) decimal
    y0 <- chunk ",y=" *> signed (pure ()) decimal
    y1 <- chunk ".." *> signed (pure ()) decimal
    z0 <- chunk ",z=" *> signed (pure ()) decimal
    z1 <- chunk ".." *> signed (pure ()) decimal
    pure (b, ((x0, y0, z0), (x1, y1, z1)))

day22 :: (Ix a, Num a) => [(Bool, ((a, a, a), (a, a, a)))] -> Int
day22 (dropWhile (not . fst) -> (True, bounds@((x0, y0, z0), (x1, y1, z1))):ins) =
    rangeSize bounds - day22
      [ (not b, ((max x0 x0', max y0 y0', max z0 z0'), (min x1 x1', min y1 y1', min z1 z1')))
      | (b, ((x0', y0', z0'), (x1', y1', z1'))) <- ins
      , x0' <= x1, x0 <= x1', y0' <= y1, y0 <= y1', z0' <= z1, z0 <= z1'
      ] + day22
      [ (b, ((x0', y0', min (z0 - 1) z0'), (x1', y1', min (z0 - 1) z1')))
      | (b, ((x0', y0', z0'), (x1', y1', z1'))) <- ins
      , z0 > z0'
      ] + day22
      [ (b, ((x0', y0', max (z1 + 1) z0'), (x1', y1', max (z1 + 1) z1')))
      | (b, ((x0', y0', z0'), (x1', y1', z1'))) <- ins
      , z1 < z1'
      ] + day22
      [ (b, ((x0', min (y0 - 1) y0', max z0 z0'), (x1', min (y0 - 1) y1', min z1 z1')))
      | (b, ((x0', y0', z0'), (x1', y1', z1'))) <- ins
      , z0 <= z1', z1 >= z0', y0 > y0'
      ] + day22
      [ (b, ((x0', max (y1 + 1) y0', max z0 z0'), (x1', max (y1 + 1) y1', min z1 z1')))
      | (b, ((x0', y0', z0'), (x1', y1', z1'))) <- ins
      , z0 <= z1', z1 >= z0', y1 < y1'
      ] + day22
      [ (b, ((min (x0 - 1) x0', max y0 y0', max z0 z0'), (min (x0 - 1) x1', min y1 y1', min z1 z1')))
      | (b, ((x0', y0', z0'), (x1', y1', z1'))) <- ins
      , z0 <= z1', z1 >= z0', y0 <= y1', y1 >= y0', x0 > x0'
      ] + day22
      [ (b, ((max (x1 + 1) x0', max y0 y0', max z0 z0'), (max (x1 + 1) x1', min y1 y1', min z1 z1')))
      | (b, ((x0', y0', z0'), (x1', y1', z1'))) <- ins
      , z0 <= z1', z1 >= z0', y0 <= y1', y1 >= y0', x1 < x1'
      ]
day22 _ = 0

day22a :: Text -> Either (ParseErrorBundle Text Void) Int
day22a input = day22 @Int . filter f <$> parse (parser `sepEndBy` newline <* eof) "" input where
    f (_, (lo, hi)) = inRange ((-50, -50, -50), (50, 50, 50)) lo && inRange ((-50, -50, -50), (50, 50, 50)) hi

day22b :: Text -> Either (ParseErrorBundle Text Void) Int
day22b input = day22 @Int <$> parse (parser `sepEndBy` newline <* eof) "" input
