module TestHomework8 (spec) where

import Data.Tree
import Test.Hspec

import Homework8.Employee (
  Employee (Emp, empFun, empName),
  GuestList (GL),
  testCompany,
  testCompany2,
 )
import Homework8.Party (glCons, maxFun, moreFun, treeFold)

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

    describe "Exercise 2" $ do
      it "treeFold" $
        treeFold (\a b -> empName a : concat b) [] testCompany
          `shouldBe` ["Stan", "Bob", "Joe", "John", "Sue", "Fred", "Sarah", "Sam"]

      it "treeFold sum" $
        treeFold
          (\a b -> empFun a + sum b)
          0
          ( Node
              (Emp "Joe" 5) -- (5, 6)
              [ Node (Emp "John" 1) [] -- (1, 0)
              , Node (Emp "Sue" 5) [] -- (5, 0)
              ]
          )
          `shouldBe` 11

      it "treeFold max" $
        treeFold
          (\a b -> max (empFun a) (maximum b))
          0
          ( Node
              (Emp "Joe" 5) -- (5, 6)
              [ Node (Emp "John" 4) [] -- (1, 0)
              , Node (Emp "Sue" 6) [] -- (5, 0)
              ]
          )
          `shouldBe` 6

    describe "Exercise 3,4" $ do
      it "maxFun" $
        maxFun
          ( Node
              (Emp "Joe" 5) -- (5, 6)
              [ Node (Emp "John" 1) [] -- (1, 0)
              , Node (Emp "Sue" 5) [] -- (5, 0)
              ]
          )
          `shouldBe` GL [Emp{empName = "John", empFun = 1}, Emp{empName = "Sue", empFun = 5}] 6

      it "maxFun testCompany " $
        maxFun testCompany
          `shouldBe` GL
            [ Emp{empName = "John", empFun = 1}
            , Emp{empName = "Sue", empFun = 5}
            , Emp{empName = "Fred", empFun = 3}
            , Emp{empName = "Sarah", empFun = 17}
            ]
            26

      it "maxFun testCompany2" $
        maxFun testCompany2
          `shouldBe` GL
            [ Emp{empName = "John", empFun = 1}
            , Emp{empName = "Sue", empFun = 5}
            , Emp{empName = "Fred", empFun = 3}
            , Emp{empName = "Sarah", empFun = 17}
            ]
            26
