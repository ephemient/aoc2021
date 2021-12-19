{-|
Module:         Day19
Description:    <https://adventofcode.com/2021/day/19 Day 19: Beacon Scanner>
-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies #-}
module Day19 (day19) where

import Control.Monad (guard, mfilter, replicateM)
import Data.Bits (setBit, testBit)
import Data.Bool (bool)
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import qualified Data.IntSet as IntSet (delete, fromDistinctAscList, member, null)
import Data.List (permutations, sortOn)
import qualified Data.Map as Map (assocs, fromListWith)
import Data.Maybe (listToMaybe)
import Data.Ord (Down(..))
import qualified Data.Set as Set (elems, fromList, intersection, size)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, Tokens, chunk, parse, sepBy1, sepEndBy1, single, skipSome)
import Text.Megaparsec.Char (alphaNumChar, newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)

parser :: (MonadParsec e s m, IsString (Tokens s), Token s ~ Char, Num a) => m [[[a]]]
parser = mfilter checkSizes $ scanner `sepBy1` newline where
    scanner = chunk "--- scanner " *> skipSome alphaNumChar *> chunk " ---" *> newline *>
        (signed (pure ()) decimal `sepBy1` single ',') `sepEndBy1` newline
    checkSizes ((x:xs):xss) = all (((==) `on` length) x) (xs ++ concat xss) && not (any null xss)
    checkSizes _ = False

allTransforms :: Int -> [[(Bool, Int)]]
allTransforms n = filter parity $ zip <$> replicateM n [False, True] <*> permutations [0..n - 1] where
    parity t = snd $ foldr decomp (0 :: Int, True) [0..n - 1] where
        decomp i k@(bits, p) = if testBit bits i then k else decomp' i bits $ not p
        decomp' i bits p = if testBit bits i then (bits, p) else decomp' j (setBit bits i) $ p == q where
            (q, j) = t !! i

applyTransform :: (Num a) => [(Bool, Int)] -> [a] -> [a]
applyTransform t point = [bool id negate s $ point !! i | (s, i) <- t]

day19 :: Text -> Either (ParseErrorBundle Text Void) (Maybe (Int, Int))
day19 input = do
    scanners <- parse parser "" input
    let size = length $ head $ head scanners
        deltas = Map.fromListWith (<>) $ do
            (i, scanner) <- zip [1..] $ tail scanners
            let delta = nubOrd $ zipWith (-) <$> scanner <*> scanner
            t <- allTransforms size
            pure (Set.fromList $ applyTransform t <$> delta, [(i, t)])
        go beacons positions remaining
          | IntSet.null remaining
          = pure (Set.size beacons, maximum [sum $ abs <$> zipWith (-) x y | x <- positions, y <- positions])
          | otherwise = do
                let delta0 = Set.fromList $ zipWith (-) <$> Set.elems beacons <*> Set.elems beacons
                (i, t) <- sortOn (Down . Set.size . Set.intersection delta0 . fst) (Map.assocs deltas) >>= snd
                guard $ IntSet.member i remaining
                let points = applyTransform t <$> scanners !! i
                position <- zipWith (-) <$> Set.elems beacons <*> points
                let beacons' = Set.fromList $ zipWith (+) position <$> points
                guard $ Set.size (Set.intersection beacons beacons') >= 12
                go (beacons <> beacons') (position:positions) $ IntSet.delete i remaining
    pure $ listToMaybe $ go (Set.fromList $ head scanners) [replicate size 0] $
        IntSet.fromDistinctAscList [1..length scanners - 1]
