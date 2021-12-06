{-# LANGUAGE OverloadedStrings #-}
module Day6Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day6 (day6a, day6b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines ["3,4,3,1,2"]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day6a example `shouldBe` Right 5934
    describe "part 2" $ do
        it "examples" $ do
            day6b example `shouldBe` Right 26984457539
