module TestHomework7 (spec) where

import Homework7.JoinList (JoinList (Append, Empty, Single), tag)

import Test.Hspec

spec :: Spec
spec = do
  describe "Homework7" $ do
    describe "Exercise 1" $ do
      it "tag empty" $
        tag Empty `shouldBe` ""

      it "tag single" $
        tag (Single "10" "20") `shouldBe` "10"

      it "tag append" $
        tag (Append "10" (Single "20" "30") (Single "40" "50")) `shouldBe` "10"
