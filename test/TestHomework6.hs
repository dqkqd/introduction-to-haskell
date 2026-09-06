module TestHomework6 (spec) where

import Homework6 (fibs1, fibs2)
import Test.Hspec

spec :: Spec
spec = do
  describe "Homework6" $ do
    describe "Exercise 1" $ do
      it "fibs1" $
        take 10 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

    describe "Exercise 2" $ do
      it "fibs2" $
        take 15 fibs2
          `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
