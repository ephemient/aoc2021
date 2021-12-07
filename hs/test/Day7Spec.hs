{-# LANGUAGE OverloadedStrings #-}
module Day7Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day7 (day7a, day7b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines ["16,1,2,0,4,2,7,1,2,14"]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day7a example `shouldBe` Right 37
    describe "part 2" $ do
        it "examples" $ do
            day7b example `shouldBe` Right 168
