{-# LANGUAGE OverloadedStrings #-}
module Day13Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day13 (day13a, day13b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "6,10"
  , "0,14"
  , "9,10"
  , "0,3"
  , "10,4"
  , "4,11"
  , "6,0"
  , "6,12"
  , "4,1"
  , "0,13"
  , "10,12"
  , "3,4"
  , "3,0"
  , "8,4"
  , "1,10"
  , "2,14"
  , "8,10"
  , "9,0"
  , ""
  , "fold along y=7"
  , "fold along x=5"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day13a example `shouldBe` Right 17
    describe "part 2" $ do
        it "examples" $ do
            day13b example `shouldBe` Right ["▓▓▓▓▓", "▓░░░▓", "▓░░░▓", "▓░░░▓", "▓▓▓▓▓"]
