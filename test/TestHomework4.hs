module TestHomework4 (spec) where

import Homework4 (fun1, fun2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Homework4" $ do
    describe "Exercise 1" $ do
      it "fun1 [4,6,8]" $
        fun1 [4, 6, 8] `shouldBe` 48
      it "fun1 []" $
        fun1 [] `shouldBe` 1

      it "fun2 10" $
        fun2 10 `shouldBe` 40
      it "fun2 9" $
        fun2 9 `shouldBe` 276
