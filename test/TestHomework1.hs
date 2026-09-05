module TestHomework1 (spec) where

import Homework1 (
  doubleEveryOther,
  hanoi,
  sumDigits,
  toDigits,
  toDigitsRev,
  validate,
 )
import Test.Hspec

spec :: Spec
spec = do
  describe "Homework1" $ do
    describe "Exercise 1" $ do
      it "toDigits 1234" $
        toDigits 1234 `shouldBe` [1, 2, 3, 4]

      it "toDigitsRev 1234" $
        toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]

      it "toDigits 0" $
        toDigits 0 `shouldBe` []

      it "toDigits -17" $
        toDigits (-17) `shouldBe` []

    describe "Exercise 2" $ do
      it "doubleEveryOther [8,7,6,5]" $
        doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]

      it "doubleEveryOther [1,2,3]" $
        doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

      it "doubleEveryOther [1]" $
        doubleEveryOther [1] `shouldBe` [1]

      it "doubleEveryOther []" $
        doubleEveryOther [] `shouldBe` []

    describe "Exercise 3" $ do
      it "sumDigits [16,7,12,5]" $
        sumDigits [16, 7, 12, 5] `shouldBe` 22

      it "sumDigits []" $
        sumDigits [] `shouldBe` 0

    describe "Exercise 4" $ do
      it "validate 4012888888881881" $
        validate 4012888888881881 `shouldBe` True

      it "validate 4012888888881881" $
        validate 4012888888881882 `shouldBe` False

    describe "Exercise 5" $ do
      it "hanoi 2 a b c" $
        hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
