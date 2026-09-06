module TestHomework6 (spec) where

import Homework6 (fibs1)
import Test.Hspec

spec :: Spec
spec = do
  describe "Homework6" $ do
    describe "Exercise 1" $ do
      it "fibs1" $
        take 10 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
