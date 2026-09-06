module TestHomework5 (spec) where

import Homework5.Calc (eval, evalStr)
import Homework5.ExprT
import Test.Hspec

spec :: Spec
spec = do
  describe "Homework5" $ do
    describe "Exercise 1" $ do
      it "eval" $
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

    describe "Exercise 2" $ do
      it "evalStr" $
        evalStr "(2+3)*4" `shouldBe` Just 20
