{-# LANGUAGE OverloadedStrings #-}
module Day1Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day1 (day1a, day1b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines ["199", "200", "208", "210", "200", "207", "240", "269", "260", "263"]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day1a example `shouldBe` Right 7
    describe "part 2" $ do
        it "examples" $ do
            day1b example `shouldBe` Right 5
