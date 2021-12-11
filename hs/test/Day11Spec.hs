{-# LANGUAGE OverloadedStrings #-}
module Day11Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day11 (day11)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "5483143223"
  , "2745854711"
  , "5264556173"
  , "6141336146"
  , "6357385478"
  , "4167524645"
  , "2176841721"
  , "6882881134"
  , "4846848554"
  , "5283751526"
  ]

spec :: Spec
spec = do
    describe "both" $ do
        it "examples" $ do
            day11 example `shouldBe` Just (1656, 195)
