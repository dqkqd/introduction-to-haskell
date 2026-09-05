module TestHomework3 (spec) where

import Homework3 (skips)
import Test.Hspec

spec :: Spec
spec = do
  describe "Exercise 1: skips" $ do
    it "skips \"ABCD\"" $
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]

    it "skips \"hello!\"" $
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

    it "skips [1]" $
      skips [1] `shouldBe` [[1]]

    it "skips [True,False]" $
      skips [True, False] `shouldBe` [[True, False], [False]]

    it "skips []" $
      skips ([] :: [Int]) `shouldBe` ([] :: [[Int]])
