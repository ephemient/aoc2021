{-|
Module:         Day17
Description:    <https://adventofcode.com/2021/day/17 Day 17: Trick Shot>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TransformListComp, TypeFamilies #-}
module Day17 (day17) where

import Data.Function (on)
import Data.Ix (inRange)
import Data.List (scanl')
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, chunk, parse)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a, Ord a) => m ((a, a), (a, a))
parser = do
    x0 <- chunk "target area: x=" *> decimal
    x1 <- chunk ".." *> decimal
    y0 <- chunk ", y=" *> signed (pure ()) decimal
    y1 <- chunk ".." *> signed (pure ()) decimal
    pure ((x0, y0), (x1, y1))

day17 :: Text -> Either (ParseErrorBundle Text Void) (Maybe (Int, Int))
day17 input = do
    bounds@((x0, y0), (x1, y1)) <- parse parser "" input
    let trajectories = do
            -- dx * (dx + 1) / 2 >= x0
            -- dx^2 + dx + 1/4 >= 2*x0 + 1/4
            -- (dx + 1/2)^2 >= 2*x0 + 1/4
            dx <- [ceiling $ sqrt (2 * fromIntegral x0 + 0.25 :: Double) - 0.5 .. x1]
            let xs = scanl' (+) 0 $ [dx, dx - 1..1] ++ repeat 0
            let finalX = dx * (dx + 1) `div` 2
            [ maximum ys
              | dy <- [y0..]
              , let (ys0, ys1) = splitAt (dy + 1) $ scanl' (+) 0 [dy, dy - 1..]
              , let ys = ys0 ++ takeWhile (>= y0) ys1
              , let finalY = dy * (dx + 1) - finalX
              , then takeWhile by finalY <= y1 || finalX <= x1 && dy <= (max `on` abs) y0 y1
              , any (inRange bounds) $ zip xs ys
              ]
    pure $ if null trajectories then Nothing else Just (maximum trajectories, length trajectories)
