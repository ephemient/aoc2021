{-# LANGUAGE OverloadedStrings #-}
module Day17Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day17 (day17)
import Test.Hspec (Spec, describe, it, shouldBe)

example :: Text
example = T.unlines ["target area: x=20..30, y=-10..-5"]

spec :: Spec
spec = do
    describe "both" $ do
        it "examples" $ do
            day17 example `shouldBe` Right (Just (45, 112))
