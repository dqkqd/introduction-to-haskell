module TestHomework5 (spec) where

import Homework5.Calc (eval)
import Homework5.ExprT
import Test.Hspec

spec :: Spec
spec = do
  describe "Homework5" $ do
    describe "Exercise 1" $ do
      it "eval" $
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
