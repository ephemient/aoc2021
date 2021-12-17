{-|
Module:         Day12
Description:    <https://adventofcode.com/2021/day/12 Day 12: Passage Pathing>
-}
{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, TypeApplications #-}
module Day12 (day12a, day12b) where

import Control.Monad (forM, guard, when)
import Control.Monad.Writer (execWriter, tell)
import Data.Bits (finiteBitSize, setBit, testBit)
import Data.Char (isUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.Graph.Inductive (Graph, Gr, lsuc, mkGraph, nodes)
import qualified Data.Map as Map ((!), (!?), fromList, toList)
import Data.Monoid (Sum(..))
import Data.Text (Text)
import qualified Data.Text as T (all, breakOn, lines, stripPrefix)
import Data.Tuple (swap)

parse :: (Graph gr) => Text -> Maybe (Int, Int, gr Text Bool)
parse input = do
    conn <- forM (T.lines input) $ \line ->
        let (a, b) = T.breakOn "-" line in (,) a <$> T.stripPrefix "-" b
    let names = Map.fromList $ flip zip [0..] $ nubOrd $ concat [[a, b] | (a, b) <- conn]
    start <- names Map.!? "start"
    end <- names Map.!? "end"
    pure
      ( start
      , end
      , mkGraph (swap <$> Map.toList names) $ do
            (a, b) <- conn
            let a' = names Map.! a
                b' = names Map.! b
            [(a', b', T.all isUpper b), (b', a', T.all isUpper a)]
      )

walk :: (Monad m, Traversable t) => (a -> m (t a)) -> a -> m ()
walk f = walk' where walk' a = f a >>= mapM_ walk'

day12 :: Bool -> Text -> Maybe Int
day12 bonus input = do
    (start, end, g) <- parse @Gr input
    when (any (>= bonusBit) $ nodes g) $ error "input too large"
    let step (state, i)
          | i == end = mempty <$ tell (Sum 1)
          | otherwise = pure $ do
                (j, big) <- lsuc g i
                guard $ j /= start
                if
                  | big -> pure (state, j)
                  | not $ testBit state j -> pure (setBit state j, j)
                  | not $ testBit state bonusBit -> pure (setBit state bonusBit, j)
                  | otherwise -> mempty
    pure $ getSum $ execWriter $ walk step (if bonus then 0 else setBit 0 bonusBit :: Int, start)
  where bonusBit = finiteBitSize (0 :: Int) - 1

day12a :: Text -> Maybe Int
day12a = day12 False

day12b :: Text -> Maybe Int
day12b = day12 True
