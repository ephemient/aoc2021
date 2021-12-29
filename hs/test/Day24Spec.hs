{-# LANGUAGE OverloadedStrings #-}
module Day24Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day24 (day24)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "inp w"
  , "mul z 31"
  , "add z w"
  , "mod z 16777216"
  ]

spec :: Spec
spec = do
    describe "both" $ do
        it "examples" $ do
            day24 example `shouldBe` Right (Just (99999993811817, 11111128657365))
