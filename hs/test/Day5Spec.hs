{-# LANGUAGE OverloadedStrings #-}
module Day5Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day5 (day5a, day5b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "0,9 -> 5,9"
  , "8,0 -> 0,8"
  , "9,4 -> 3,4"
  , "2,2 -> 2,1"
  , "7,0 -> 7,4"
  , "6,4 -> 2,0"
  , "0,9 -> 2,9"
  , "3,4 -> 1,4"
  , "0,0 -> 8,8"
  , "5,5 -> 8,2"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day5a example `shouldBe` Right 5
    describe "part 2" $ do
        it "examples" $ do
            day5b example `shouldBe` Right 12
