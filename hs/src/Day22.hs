{-|
Module:         Day22
Description:    <https://adventofcode.com/2021/day/22 Day 22: Reactor Reboot>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeApplications, TypeFamilies, ViewPatterns #-}
module Day22 (day22a, day22b) where

import Control.Monad (guard)
import Data.Ix (Ix, inRange, rangeSize)
import Data.Maybe (mapMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, (<|>), chunk, eof, parse, sepEndBy)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m (Bool, ((a, a), (a, a), (a, a)))
parser = do
    b <- (True <$ chunk "on") <|> (False <$ chunk "off")
    x0 <- chunk " x=" *> signed (pure ()) decimal
    x1 <- chunk ".." *> signed (pure ()) decimal
    y0 <- chunk ",y=" *> signed (pure ()) decimal
    y1 <- chunk ".." *> signed (pure ()) decimal
    z0 <- chunk ",z=" *> signed (pure ()) decimal
    z1 <- chunk ".." *> signed (pure ()) decimal
    pure (b, ((x0, x1), (y0, y1), (z0, z1)))

day22 :: (Ix a, Num a) => Bool -> [(Bool, ((a, a), (a, a), (a, a)))] -> Int
day22 on (dropWhile ((/= on) . fst) -> (_, ((x0, x1), (y0, y1), (z0, z1))):ins) =
    rangeSize ((x0, y0, z0), (x1, y1, z1))
        - day22' (not on) (clip x0 x1) (clip y0 y1) (clip z0 z1)
        + day22' on Just Just (below z0)
        + day22' on Just Just (above z1)
        + day22' on Just (below y0) (clip z0 z1)
        + day22' on Just (above y1) (clip z0 z1)
        + day22' on (below x0) (clip y0 y1) (clip z0 z1)
        + day22' on (above x1) (clip y0 y1) (clip z0 z1)
  where
    day22' on' f g h = day22 on' $ flip mapMaybe ins $ \(b, (x, y, z)) ->
        (,) b <$> ((,,) <$> f x <*> g y <*> h z)
    above hi (u, v) = (max (hi + 1) u, v) <$ guard (v > hi)
    below lo (u, v) = (u, min (lo - 1) v) <$ guard (u < lo)
    clip lo hi (u, v) = (max lo u, min hi v) <$ guard (u <= hi && v >= lo)
day22 _ _ = 0

day22a :: Text -> Either (ParseErrorBundle Text Void) Int
day22a input = day22 @Int True . filter f <$> parse (parser `sepEndBy` newline <* eof) "" input where
    f (_, ((x0, x1), (y0, y1), (z0, z1))) =
        inRange ((-50, -50, -50), (50, 50, 50)) (x0, y0, z0) &&
        inRange ((-50, -50, -50), (50, 50, 50)) (x1, y1, z1)

day22b :: Text -> Either (ParseErrorBundle Text Void) Int
day22b input = day22 @Int True <$> parse (parser `sepEndBy` newline <* eof) "" input
