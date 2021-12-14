{-# LANGUAGE OverloadedStrings #-}
module Day14Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day14 (day14a, day14b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "NNCB"
  , ""
  , "CH -> B"
  , "HH -> N"
  , "CB -> H"
  , "NH -> C"
  , "HB -> C"
  , "HC -> B"
  , "HN -> C"
  , "NN -> C"
  , "BH -> H"
  , "NC -> B"
  , "NB -> B"
  , "BN -> B"
  , "BB -> N"
  , "BC -> B"
  , "CC -> N"
  , "CN -> C"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day14a example `shouldBe` Just 1588
    describe "part 2" $ do
        it "examples" $ do
            day14b example `shouldBe` Just 2188189693529
