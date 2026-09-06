module TestHomework4 (spec) where

import Homework4 (
  Tree (Leaf, Node),
  foldTree,
  fun1,
  fun2,
  map',
  sieveSundaram,
  xor,
 )
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

    describe "Exercise 2" $ do
      it "foldTree ABCDEFGHIJ" $
        foldTree "ABCDEFGHIJ"
          `shouldBe` Node
            3
            ( Node
                2
                (Node 1 (Node 0 Leaf 'D' Leaf) 'G' Leaf)
                'I'
                (Node 1 (Node 0 Leaf 'A' Leaf) 'E' Leaf)
            )
            'J'
            ( Node
                2
                (Node 1 (Node 0 Leaf 'B' Leaf) 'F' Leaf)
                'H'
                (Node 0 Leaf 'C' Leaf)
            )

    describe "Exercise 3" $ do
      it "xor [False, True, False]" $
        xor [False, True, False] `shouldBe` True

      it "xor [False, True, False, False, True]" $
        xor [False, True, False, False, True] `shouldBe` False

      it "xor []" $
        xor [] `shouldBe` False

      it "map' (+)" $
        map'
          (+ 1)
          ([1, 2, 3, 4, 5] :: [Integer])
          `shouldBe` ([2, 3, 4, 5, 6] :: [Integer])

      it "map' (-)" $
        map'
          (subtract 1)
          ([1, 2, 3, 4, 5] :: [Integer])
          `shouldBe` ([0, 1, 2, 3, 4] :: [Integer])

      it "sieveSundaram" $
        sieveSundaram 20
          `shouldBe` [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41]
