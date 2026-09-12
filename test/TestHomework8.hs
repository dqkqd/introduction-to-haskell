module TestHomework8 (spec) where

import Homework8.Employee (Employee (Emp), GuestList (GL))
import Homework8.Party (glCons)
import Test.Hspec

spec :: Spec
spec = do
  describe "Homework8" $ do
    describe "Exercise 1" $ do
      it "glCons" $
        glCons (Emp "One" 1) (GL [Emp "Two" 2] 2)
          `shouldBe` GL [Emp "One" 1, Emp "Two" 2] 3
