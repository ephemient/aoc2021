{-# LANGUAGE OverloadedStrings #-}
module Day10Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day10 (day10a, day10b)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "[({(<(())[]>[[{[]{<()<>>"
  , "[(()[<>])]({[<{<<[]>>("
  , "{([(<{}[<>[]}>{[]{[(<()>"
  , "(((({<>}<{<{<>}{[]{[]{}"
  , "[[<[([]))<([[{}[[()]]]"
  , "[{[{({}]{}}([{[{{{}}([]"
  , "{<[[]]>}<{[{[{[]{()[[[]"
  , "[<(<(<(<{}))><([]([]()"
  , "<{([([[(<>()){}]>(<<{{"
  , "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day10a example `shouldBe` 26397
    describe "part 2" $ do
        it "examples" $ do
            day10b example `shouldBe` Just 288957
