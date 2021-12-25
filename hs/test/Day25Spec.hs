{-# LANGUAGE OverloadedStrings #-}
module Day25Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day25 (day25)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines
  [ "v...>>.vv>"
  , ".vv>>.vv.."
  , ">>.>v>...v"
  , ">>v>>.>.v."
  , "v>v.vv.v.."
  , ">.>>..v..."
  , ".vv..>.>v."
  , "v.v..>>v.v"
  , "....v..v.>"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day25 example `shouldBe` Just 58
