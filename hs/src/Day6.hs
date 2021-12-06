{-|
Module:         Day6
Description:    <https://adventofcode.com/2021/day/6 Day 6: Lanternfish>
-}
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module Day6 (day6a, day6b) where

import Common (readEntire)
import Control.Monad (forM_, replicateM_)
import Control.Monad.ST (runST)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T (dropAround, splitOn)
import qualified Data.Text.Read as T (decimal)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic.Mutable as MV (foldl', modify, move, read, replicate, slice, write)
import qualified Data.Vector.Primitive as Primitive (Vector)
import Data.Vector.Generic.New (New)
import qualified Data.Vector.Generic.New as New (create, run)

day6 :: (Integral a, Vector v a) => New v a -> Int -> Text -> Either String a
day6 new times input = do
    nums <- mapM (readEntire T.decimal) $ T.splitOn "," $ T.dropAround isSpace input
    pure $ runST $ do
        fishes <- New.run new
        forM_ nums $ MV.modify fishes succ
        replicateM_ times $ do
            zero <- MV.read fishes 0
            MV.move (MV.slice 0 8 fishes) (MV.slice 1 8 fishes)
            MV.modify fishes (+ zero) 6
            MV.write fishes 8 zero
        MV.foldl' (+) 0 fishes

day6a :: Text -> Either String Int
day6a = day6 (New.create @Primitive.Vector $ MV.replicate 9 0) 80

day6b :: Text -> Either String Int
day6b = day6 (New.create @Primitive.Vector $ MV.replicate 9 0) 256
