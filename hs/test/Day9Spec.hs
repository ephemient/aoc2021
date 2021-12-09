{-# LANGUAGE OverloadedStrings #-}
module Day9Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day9 (day9a, day9b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "2199943210"
  , "3987894921"
  , "9856789892"
  , "8767896789"
  , "9899965678"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day9a example `shouldBe` 15
    describe "part 2" $ do
        it "examples" $ do
            day9b example `shouldBe` 1134
