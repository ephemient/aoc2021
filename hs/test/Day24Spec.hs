{-# LANGUAGE OverloadedStrings #-}
module Day24Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day24 (day24a, day24b)
import Test.Hspec (Spec, describe, it, shouldReturn)

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
    describe "part 1" $ do
        it "examples" $ do
            day24a example `shouldReturn` Right (Just 99999993811817)
    describe "part 2" $ do
        it "examples" $ do
            day24b example `shouldReturn` Right (Just 11111128657365)
