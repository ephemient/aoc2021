{-# LANGUAGE OverloadedStrings #-}
module Day21Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day21 (day21a, day21b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "Player 1 starting position: 4"
  , "Player 2 starting position: 8"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day21a example `shouldBe` Right 739785
    describe "part 2" $ do
        it "examples" $ do
            day21b example `shouldBe` Right 444356092776315
