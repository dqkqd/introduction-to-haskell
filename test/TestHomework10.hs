module TestHomework10 (spec) where

import Test.Hspec

import Homework10.AParser (posInt, runParser)

spec :: Spec
spec = do
  describe "Homework10" $ do
    describe "Exercise 1" $ do
      it "Functors for Parser" $
        runParser ((+ 10) `fmap` posInt) "5A" `shouldBe` Just (15, "A")
