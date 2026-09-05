module TestHomework1 (spec) where

import Homework1 (toDigits, toDigitsRev)
import Test.Hspec

spec :: Spec
spec = describe "Homework1" $ do
  it "toDigits 1234" $
    toDigits 1234 `shouldBe` [1, 2, 3, 4]
  it "toDigitsRev 1234" $
    toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]
  it "toDigits 0" $
    toDigits 0 `shouldBe` []
  it "toDigits -17" $
    toDigits (-17) `shouldBe` []
