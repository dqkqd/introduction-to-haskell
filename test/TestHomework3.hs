module TestHomework3 (spec) where

import Homework3 (histogram, localMaxima, skips)
import Test.Hspec

spec :: Spec
spec = do
  describe "Homework3" $ do
    describe "Exercise 1: skips" $ do
      it "skips \"ABCD\"" $
        skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]

      it "skips \"hello!\"" $
        skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

      it "skips [1]" $
        skips [1 :: Int] `shouldBe` [[1]]

      it "skips [True,False]" $
        skips [True, False] `shouldBe` [[True, False], [False]]

      it "skips []" $
        skips ([] :: [Int]) `shouldBe` ([] :: [[Int]])

    describe "Exercise 2: localMaxima" $ do
      it "localMaxima [2,9,5,6,1]" $
        localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]

      it "localMaxima [2,3,4,1,5]" $
        localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]

      it "localMaxima [1,2,3,4,5]" $
        localMaxima [1, 2, 3, 4, 5] `shouldBe` []

    describe "Exercise 3: histogram" $ do
      it "histogram [1,1,1,5]" $
        histogram [1, 1, 1, 5]
          `shouldBe` unlines
            [ " *        "
            , " *        "
            , " *   *    "
            , "=========="
            , "0123456789"
            ]

      it "histogram [1,4,5,4,6,6,3,4,2,4,9]" $
        histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9]
          `shouldBe` unlines
            [ "    *     "
            , "    *     "
            , "    * *   "
            , " ******  *"
            , "=========="
            , "0123456789"
            ]
