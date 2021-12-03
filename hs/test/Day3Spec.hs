{-# LANGUAGE OverloadedStrings #-}
module Day3Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day3 (day3a, day3b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day3a example `shouldBe` Right 198
    describe "part 2" $ do
        it "examples" $ do
            day3b example `shouldBe` Right 230
