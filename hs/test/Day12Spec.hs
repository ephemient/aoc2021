{-# LANGUAGE OverloadedStrings #-}
module Day12Spec (spec) where

import Data.Text (Text)
import qualified Data.Text as T (unlines)
import Day12 (day12a, day12b)
import Test.Hspec (Spec, describe, it, shouldBe)

example1, example2, example3 :: Text
example1 = T.unlines
  [ "start-A"
  , "start-b"
  , "A-c"
  , "A-b"
  , "b-d"
  , "A-end"
  , "b-end"
  ]
example2 = T.unlines
  [ "dc-end"
  , "HN-start"
  , "start-kj"
  , "dc-start"
  , "dc-HN"
  , "LN-dc"
  , "HN-end"
  , "kj-sa"
  , "kj-HN"
  , "kj-dc"
  ]
example3 = T.unlines
  [ "fs-end"
  , "he-DX"
  , "fs-he"
  , "start-DX"
  , "pj-DX"
  , "end-zg"
  , "zg-sl"
  , "zg-pj"
  , "pj-he"
  , "RW-he"
  , "fs-DX"
  , "pj-RW"
  , "zg-RW"
  , "start-pj"
  , "he-WI"
  , "zg-he"
  , "pj-fs"
  , "start-RW"
  ]

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day12a example1 `shouldBe` Just 10
            day12a example2 `shouldBe` Just 19
            day12a example3 `shouldBe` Just 226
    describe "part 2" $ do
        it "examples" $ do
            day12b example1 `shouldBe` Just 36
            day12b example2 `shouldBe` Just 103
            day12b example3 `shouldBe` Just 3509
