{-# LANGUAGE OverloadedStrings #-}
module Day2Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day2 (day2a, day2b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines ["forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2"]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day2a example `shouldBe` Right 150
    describe "part 2" $ do
        it "examples" $ do
            day2b example `shouldBe` Right 900
