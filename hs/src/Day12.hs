{-|
Module:         Day12
Description:    <https://adventofcode.com/2021/day/12 Day 12: Passage Pathing>
-}
{-# LANGUAGE FlexibleContexts, MultiWayIf, OverloadedStrings, TypeApplications #-}
module Day12 (day12a, day12b) where

import Control.Monad (forM, guard)
import Control.Monad.Writer (execWriter, tell)
import Data.Char (isUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor (($>))
import Data.Graph.Inductive
import qualified Data.IntSet as IntSet (empty, insert, notMember)
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

walk :: (Monad m, Traversable t) => (s -> a -> m (t (s, a))) -> s -> a -> m ()
walk f = curry walk' where walk' (s, a) = f s a >>= mapM_ walk'

day12a :: Text -> Maybe Int
day12a input = do
    (start, end, g) <- parse @Gr input
    let step used src
          | src == end = tell (Sum 1) $> mempty
          | otherwise = pure $ do
                (next, big) <- lsuc g src
                guard $ next /= start
                if
                  | big -> pure (used, next)
                  | IntSet.notMember next used -> pure (IntSet.insert next used, next)
                  | otherwise -> mempty
    pure $ getSum $ execWriter $ walk step IntSet.empty start

day12b :: Text -> Maybe Int
day12b input = do
    (start, end, g) <- parse @Gr input
    let step s@(used, bonus) src
          | src == end = tell (Sum 1) $> mempty
          | otherwise = pure $ do
                (next, big) <- lsuc g src
                guard $ next /= start
                if
                  | big -> pure (s, next)
                  | IntSet.notMember next used -> pure ((IntSet.insert next used, bonus), next)
                  | bonus -> pure ((used, False), next)
                  | otherwise -> mempty
    pure $ getSum $ execWriter $ walk step (IntSet.empty, True) start
