module TestHomework6 (spec) where

import Homework6 (
  fibs1,
  fibs2,
  streamFromSeed,
  streamMap,
  streamRepeat,
  streamToList,
 )
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

    describe "Exercise 3" $ do
      it "streamToList" $
        take 10 (streamToList $ streamRepeat (5 :: Integer))
          `shouldBe` replicate 10 5

    describe "Exercise 4" $ do
      it "streamRepeat" $
        show (streamRepeat (5 :: Integer))
          `shouldBe` "Stream[5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,...]"

      it "streamMap" $
        show (streamMap (+ 1) $ streamRepeat (5 :: Integer))
          `shouldBe` "Stream[6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,...]"

      it "streamFromSeed" $
        show (streamFromSeed (+ 2) (1 :: Integer))
          `shouldBe` "Stream[1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,...]"
