{-# LANGUAGE OverloadedStrings #-}
module Day15Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day15 (day15a, day15b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "1163751742"
  , "1381373672"
  , "2136511328"
  , "3694931569"
  , "7463417111"
  , "1319128137"
  , "1359912421"
  , "3125421639"
  , "1293138521"
  , "2311944581"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day15a example `shouldBe` Just 40
    describe "part 2" $ do
        it "examples" $ do
            day15b example `shouldBe` Just 315
