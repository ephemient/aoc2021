{-|
Module:         Day17
Description:    <https://adventofcode.com/2021/day/17 Day 17: Trick Shot>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Day17 (day17) where

import Control.Monad (guard)
import Data.Ix (inRange)
import qualified Data.IntMap as IntMap ((!?), empty, insertWith)
import qualified Data.IntSet as IntSet (findMax, null, singleton, size, unions)
import Data.List (foldl', scanl')
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes)
import Data.Semigroup (Max(..), Sum(..), sconcat)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, chunk, parse)
import Text.Megaparsec.Char.Lexer (decimal)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a, Ord a) => m ((a, a), (a, a))
parser = do
    x0 <- chunk "target area: x=" *> decimal
    x1 <- chunk ".." *> decimal
    y0 <- chunk ", y=-" *> (negate <$> decimal)
    y1 <- chunk "..-" *> (negate <$> decimal)
    pure ((x0, y0), (x1, y1))

-- $> day17 $ Data.Text.pack "target area: x=20..30, y=-10..-5"
--
-- $> day17 <$> Data.Text.IO.readFile "day17.txt"
day17 :: Text -> Either (ParseErrorBundle Text Void) (Int, Int)
day17 input = do
    ((x0, y0), (x1, y1)) <- parse parser "" input
    let (maxT, dyHits) = foldl' f (0, IntMap.empty) [y0 .. -y0]
        f k dy = foldl' (g dy) k $ zip [0..] $ takeWhile (>= y0) $ scanl' (+) 0 [dy, dy - 1..]
        g dy k@(maxT', m) (t, y)
          | inRange (y0, y1) y = (max maxT' t, IntMap.insertWith (<>) t (IntSet.singleton dy) m)
          | otherwise = k
        (Max maxDy, Sum count) = sconcat $ (Max 0, Sum 0) :| do
            dx <- [ceiling (sqrt (2 * fromIntegral x0 + 0.25 :: Double) - 0.5)..x1]
            let dys = IntSet.unions $ catMaybes
                  [ dyHits IntMap.!? t
                  | (t, x) <- zip [0..maxT] $ takeWhile (<= x1) $ scanl' (+) 0 $ [dx, dx - 1..1] ++ repeat 0
                  , inRange (x0, x1) x
                  ]
            guard $ not $ IntSet.null dys
            pure (Max $ IntSet.findMax dys, Sum $ IntSet.size dys)
    pure (maxDy * (maxDy + 1) `div` 2, count)
