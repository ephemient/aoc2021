{-# LANGUAGE OverloadedStrings #-}
module Day23Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day23 (day23a, day23b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "#############"
  , "#...........#"
  , "###B#C#B#D###"
  , "  #A#D#C#A#"
  , "  #########"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day23a example `shouldBe` Just 12521
    describe "part 2" $ do
        it "examples" $ do
            day23b example `shouldBe` Just 44169
