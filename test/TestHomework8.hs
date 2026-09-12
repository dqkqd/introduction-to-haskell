module TestHomework8 (spec) where

import Homework8.Employee (Employee (Emp), GuestList (GL))
import Homework8.Party (glCons, moreFun)
import Test.Hspec

spec :: Spec
spec = do
  describe "Homework8" $ do
    describe "Exercise 1" $ do
      it "glCons" $
        glCons (Emp "One" 1) (GL [Emp "Two" 2] 2)
          `shouldBe` GL [Emp "One" 1, Emp "Two" 2] 3

      it "Monoid GuestList empty" $
        (mempty :: GuestList)
          `shouldBe` GL ([] :: [Employee]) 0

      it "Monoid GuestList <>" $
        (GL [Emp "One" 1] 1 <> GL [Emp "Two" 2] 2)
          `shouldBe` GL [Emp "One" 1, Emp "Two" 2] 3

      it "moreFun a < b" $
        moreFun (GL [Emp "One" 1] 1) (GL [Emp "Two" 2] 2)
          `shouldBe` GL [Emp "Two" 2] 2

      it "moreFun a > b" $
        moreFun (GL [Emp "Two" 2] 2) (GL [Emp "One" 1] 1)
          `shouldBe` GL [Emp "Two" 2] 2
