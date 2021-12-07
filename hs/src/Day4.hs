{-|
Module:         Day4
Description:    <https://adventofcode.com/2021/day/4 Day 4: Giant Squid>
-}
{-# LANGUAGE TypeFamilies #-}
module Day4 (day4) where

import Control.Arrow ((&&&))
import Data.Functor (($>))
import qualified Data.IntMap as IntMap ((!?), fromListWith)
import Data.List (transpose)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Semigroup (Max(Max), Min(Min), sconcat)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec, ParseErrorBundle, Token, count, eof, parse, sepBy, sepBy1, sepEndBy, skipSome)
import Text.Megaparsec.Char (char, hspace, hspace1, newline)
import Text.Megaparsec.Char.Lexer (decimal)

parser' :: (MonadParsec e s m, Token s ~ Char) => m ([Int], [[[Int]]])
parser' = do
    draws <- decimal `sepBy` char ',' <* skipSome newline
    boards <- board `sepEndBy` newline
    eof $> (draws, boards)
  where
    board = do
        first <- hspace *> decimal `sepBy1` hspace1 <* newline
        let width = length first
        rest <- line width `sepEndBy` newline
        pure $ first:rest
    line n = (:) <$> (hspace *> decimal) <*> count (n - 1) (hspace1 *> decimal)

day4 :: Text -> Either (ParseErrorBundle Text Void) (Maybe (Int, Int))
day4 input = do
    (draws, boards) <- parse parser' "" input
    let drawTurns = IntMap.fromListWith const $ zip draws [0 :: Int ..]
        scoreBoard board = do
            let turns = fmap (drawTurns IntMap.!?) <$> board
                rows = mapMaybe (fmap maximum . sequence) turns
                cols = mapMaybe (fmap maximum . sequence) $ transpose turns
            turn <- minimum <$> nonEmpty (rows ++ cols)
            let remaining = sum
                  [ value
                  | row <- zip board turns
                  , (value, maybeTurn) <- uncurry zip row
                  , maybe True (> turn) maybeTurn
                  ]
            pure (turn, draws !! turn * remaining)
    pure $ do
        scores <- nonEmpty $ mapMaybe scoreBoard boards
        let (Min (_, minScore), Max (_, maxScore)) = sconcat $ (Min &&& Max) <$> scores
        pure (minScore, maxScore)
