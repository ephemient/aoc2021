{-|
Module:         Day23
Description:    <https://adventofcode.com/2021/day/23 Day 23: Amphipod>
-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeApplications, ViewPatterns #-}
module Day23 (day23a, day23b) where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad (guard)
import Data.Char (isAlpha, ord)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Heap (FstMinPolicy)
import qualified Data.Heap as Heap (insert, singleton, view)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap ((!), (!?), assocs, delete, elems, fromList, fromListWith, insert, intersectionWith, member, notMember)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet (delete, elems, fromList, insert, singleton)
import Data.List (group, partition, sort)
import Data.Map (Map)
import qualified Data.Map as Map ((!), (!?), elems, fromList, fromListWith, insert, singleton)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Ord (Down(..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set (elems, empty, fromList, insert, member, notMember, singleton, size)
import Data.Text (Text)
import qualified Data.Text as T (filter, lines, unlines, unpack)

data Diagram a b = Diagram
  { states :: IntSet
  , owners :: IntMap a
  , goals :: Map a IntSet
  , transitions :: IntMap (Set (b, Int, IntSet))
  , initial :: IntMap a
  }
  deriving (Show)

parse :: (Ord a) => (Char -> a) -> Text -> Diagram a Int
parse read1 input = Diagram
  { states = IntSet.fromList $ Map.elems usableSpaces
  , owners = IntMap.fromList $ do
        ((x, ys), c) <- zip (IntMap.assocs startingGroups) $
            map head $ group $ sort $ T.unpack $ T.filter isAlpha input
        y <- ys
        pure (usableSpaces Map.! (y, x), read1 c)
  , goals = Map.fromListWith (<>) $ do
        ((x, ys), c) <- zip (IntMap.assocs startingGroups) $
            map head $ group $ sort $ T.unpack $ T.filter isAlpha input
        y <- ys
        pure (read1 c, IntSet.singleton $ usableSpaces Map.! (y, x))
  , transitions = IntMap.fromListWith (<>) $ do
        p <- startingSpaces
        let i = usableSpaces Map.! p
        (v, q) <- dfs Set.empty p
        j <- maybeToList $ usableSpaces Map.!? q
        guard $ i /= j
        let blockers = IntSet.fromList $ mapMaybe (usableSpaces Map.!?) $ Set.elems v
        [ (i, Set.singleton (Set.size v, j, IntSet.insert j $ IntSet.delete i blockers))
          , (j, Set.singleton (Set.size v, i, blockers))
          ]
  , initial = IntMap.fromList
        [(usableSpaces Map.! p, read1 $ input' !! y !! x) | p@(y, x) <- startingSpaces]
  } where
    input' = T.unpack <$> T.lines input
    openSpaces = [(y, x) | (y, line) <- zip [0..] input', (x, c) <- zip [0..] line, c == '.']
    startingSpaces = [(y, x) | (y, line) <- zip [0..] input', (x, c) <- zip [0..] line, isAlpha c]
    startingGroups = IntMap.fromListWith (<>) [(x, [y]) | (y, x) <- startingSpaces]
    allSpaces = Set.fromList $ openSpaces ++ startingSpaces
    usableSpaces = Map.fromList $ flip zip [0..] $
        filter ((`IntMap.notMember` startingGroups) . snd) openSpaces ++ startingSpaces
    dfs v p@(y, x) = (v, p) : concatMap (dfs (Set.insert p v))
        (filter ok [(y - 1, x), (y, x  - 1), (y, x + 1), (y + 1, x)]) where
        ok q = q `Set.notMember` v && q `Set.member` allSpaces

solve :: (Alternative f, Ord a, Num b, Ord b) => (a -> b) -> Diagram a b -> f b
solve weight Diagram { .. } =
    go (Map.singleton initial 0) $ Heap.singleton ((Down 0, 0), initial) where
    estimate c state =
      ( Down $ length $ filter id $ IntMap.elems $ IntMap.intersectionWith (/=) owners state
      , (+ c) $ sum $ Map.fromListWith min $ do
            (i, transitions') <- IntMap.assocs transitions
            owner <- maybeToList $ owners IntMap.!? i
            (d, j, _) <- Set.elems transitions'
            guard $ state IntMap.!? j == Just owner
            pure (j, if owners IntMap.!? j == Just owner then 0 else d * weight owner)
      )
    go v (Heap.view @FstMinPolicy -> Just ((_, state), heap))
      | state == owners = pure c <|> go v' heap'
      | otherwise = go v' heap'
      where
        c = v Map.! state
        (priorityMoves, otherMoves) = partition isPriority $ do
            (i, a) <- IntMap.assocs state
            (d, j, blockers) <- Set.elems $ transitions IntMap.! i
            guard $ all (`IntMap.notMember` state) $ IntSet.elems blockers
            guard $ owners IntMap.!? i /= Just a ||
                any ((/= Just a) . (state IntMap.!?)) (IntSet.elems $ goals Map.! a)
            guard $ fromMaybe True $ do
                owner <- owners IntMap.!? j
                pure $ (a == owner) &&
                    all (maybe True (== owner) . (state IntMap.!?)) (IntSet.elems $ goals Map.! a)
            let c' = c + d * weight a
                state' = IntMap.insert j a $ IntMap.delete i state
            (i, j, c', state') <$ guard (maybe True (> c') $ v Map.!? state')
        isPriority (i, j, _, _) = i `IntMap.notMember` owners && j `IntMap.member` owners
        moves
          | null priorityMoves = otherMoves
          | otherwise = [maximumBy (comparing $ \(_, j, _, _) -> j) priorityMoves]
        v' = foldr (\(_, _, c', state') -> Map.insert state' c') v moves
        heap' = foldr (\(_, _, c', state') -> Heap.insert (estimate c' state', state')) heap moves
    go _ _ = empty

day23a :: Text -> Maybe Int
day23a = solve (10 ^) . parse ((subtract `on` ord) 'A')

day23b :: Text -> Maybe Int
day23b input = solve (10 ^) $ parse ((subtract `on` ord) 'A') $ T.unlines $ pre ++ mid ++ post where
    (pre, post) = splitAt 3 $ T.lines input
    mid = ["  #D#C#B#A#", "  #D#B#A#C"]
