module TestHomework7 (spec) where

import Homework7.JoinList (
  JoinList (Append, Empty, Single),
  indexJ,
  jlToList,
  tag,
  (+++),
 )
import Homework7.Sized (Size)

import Control.Monad (forM_)
import Test.Hspec
import Text.Printf (printf)

spec :: Spec
spec = do
  describe "Homework7" $ do
    describe "Exercise 1" $ do
      it "tag empty" $
        tag Empty `shouldBe` ""

      it "tag single" $
        tag (Single "10" "20") `shouldBe` "10"

      it "tag append" $
        tag (Append "10" (Single "20" "30") (Single "40" "50")) `shouldBe` "10"

      it "(+++)" $
        Single "20" "30" +++ Single "40" "50"
          `shouldBe` Append "2040" (Single "20" "30") (Single "40" "50")

    describe "Exercise 2" $ do
      let singleJ = Single (1 :: Size)
      let append = (singleJ "1" +++ singleJ "2") +++ (singleJ "3" +++ singleJ "4")
      let cases =
            [ (0 :: Int, Empty, Nothing)
            , (1 :: Int, Empty, Nothing)
            , -- single
              (0 :: Int, Single (1 :: Size) "10", Just "10")
            , (1 :: Int, Single 1 "10", Nothing)
            , (2 :: Int, Single 1 "10", Nothing)
            , (0 :: Int, Single 2 "10", Nothing)
            , -- append
              (0 :: Int, append, Just "1")
            , (1 :: Int, append, Just "2")
            , (2 :: Int, append, Just "3")
            , (3 :: Int, append, Just "4")
            , (4 :: Int, append, Nothing)
            , (5 :: Int, append, Nothing)
            ]
      forM_ cases $ \(i, tree, expected) ->
        it
          ( printf
              "indexJ index=%d list=%s, expected=%s"
              i
              (show (jlToList tree))
              (show expected)
          ) $
          indexJ i tree `shouldBe` expected
