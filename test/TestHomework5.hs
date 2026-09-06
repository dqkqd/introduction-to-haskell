module TestHomework5 (spec) where

import Homework5.Calc (
  Expr (add, lit, mul),
  MinMax (MinMax),
  Mod7 (Mod7),
  compile,
  eval,
  evalStr,
 )
import Homework5.ExprT
import Homework5.Parser (parseExp)
import Homework5.StackVM (stackVM)
import Homework5.StackVM qualified as VM
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

    describe "Exercise 3" $ do
      it "lit add mul" $
        mul (add (lit 2) (lit 3)) (lit 4) `shouldBe` Mul (Add (Lit 2) (Lit 3)) (Lit 4)

    describe "Exercise 4" $ do
      it "integer" $
        let
          value :: Maybe Integer
          value = parseExp lit add mul "(3 * -4) + 5"
         in
          value `shouldBe` Just (-7)

      it "bool" $
        let
          value :: Maybe Bool
          value = parseExp lit add mul "(3 * -4) + 5"
         in
          value `shouldBe` Just True

      it "minmax" $
        let
          value :: Maybe MinMax
          value = parseExp lit add mul "(3 * -4) + 5"
         in
          value `shouldBe` Just (MinMax 5)

      it "mod7" $
        let
          value :: Maybe Mod7
          value = parseExp lit add mul "(3 * -4) + 5"
         in
          value `shouldBe` Just (Mod7 0)

    describe "Exercise 5" $ do
      it "compile and run on the stack VM" $
        case compile "(2 + 3) * 4" of
          Just prog -> case stackVM prog of
            Right (VM.IVal v) -> v `shouldBe` 20
            other -> expectationFailure ("stackVM: " ++ show other)
          Nothing -> expectationFailure "compile returned Nothing"
