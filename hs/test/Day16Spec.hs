{-# LANGUAGE OverloadedStrings #-}
module Day16Spec (spec) where

import Day16 (day16a, day16b)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "part 1" $ do
        it "examples" $ do
            day16a "8A004A801A8002F478" `shouldBe` Right 16
            day16a "620080001611562C8802118E34" `shouldBe` Right 12
            day16a "C0015000016115A2E0802F182340" `shouldBe` Right 23
            day16a "A0016C880162017C3686B18A3D4780" `shouldBe` Right 31
    describe "part 2" $ do
        it "examples" $ do
            day16b "C200B40A82" `shouldBe` Right (Just 3)
            day16b "04005AC33890" `shouldBe` Right (Just 54)
            day16b "880086C3E88112" `shouldBe` Right (Just 7)
            day16b "CE00C43D881120" `shouldBe` Right (Just 9)
            day16b "D8005AC2A8F0" `shouldBe` Right (Just 1)
            day16b "F600BC2D8F" `shouldBe` Right (Just 0)
            day16b "9C005AC2F8F0" `shouldBe` Right (Just 0)
            day16b "9C0141080250320F1802104A08" `shouldBe` Right (Just 1)
