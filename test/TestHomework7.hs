module TestHomework7 (spec) where

import Homework7.JoinList (
  JoinList (Append, Empty, Single),
  dropJ,
  indexJ,
  jlToList,
  tag,
  takeJ,
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

      let cases =
            [ (Empty, Empty, Empty)
            , (Single "1" "2", Empty, Single "1" "2")
            , (Empty, Single "1" "2", Single "1" "2")
            , (Single "1" "2", Single "3" "4", Append "13" (Single "1" "2") (Single "3" "4"))
            ,
              ( Append "13" (Single "1" "2") (Single "3" "4")
              , Append "57" (Single "5" "6") (Single "7" "8")
              , Append
                  "1357"
                  (Append "13" (Single "1" "2") (Single "3" "4"))
                  (Append "57" (Single "5" "6") (Single "7" "8"))
              )
            ]
      forM_ cases $ \(lhs, rhs, expected) ->
        it
          ( printf
              "%s +++ %s = %s"
              (show lhs)
              (show lhs)
              (show rhs)
          )
          $ (lhs +++ rhs) `shouldBe` expected

    describe "Exercise 2" $ do
      let singleJ = Single (1 :: Size)
      let append = (singleJ "1" +++ singleJ "2") +++ (singleJ "3" +++ singleJ "4")

      describe "indexJ" $ do
        let cases =
              [ (0 :: Int, Empty, Nothing)
              , (1 :: Int, Empty, Nothing)
              , -- single
                (0 :: Int, Single 1 "10", Just "10")
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
            )
            $ indexJ i tree `shouldBe` expected

      describe "dropJ" $ do
        let cases =
              [ (0 :: Int, Empty, [])
              , (1 :: Int, Empty, [])
              , -- single
                (0 :: Int, Single 1 "10", ["10"])
              , (1 :: Int, Single 1 "10", [])
              , (2 :: Int, Single 1 "10", [])
              , -- append
                (0 :: Int, append, ["1", "2", "3", "4"])
              , (1 :: Int, append, ["2", "3", "4"])
              , (2 :: Int, append, ["3", "4"])
              , (3 :: Int, append, ["4"])
              , (4 :: Int, append, [])
              , (5 :: Int, append, [])
              ]
        forM_ cases $ \(i, tree, expected) ->
          it
            ( printf
                "dropJ n=%d list=%s, expected=%s"
                i
                (show (jlToList tree))
                (show expected)
            )
            $ jlToList (dropJ i tree) `shouldBe` expected

      describe "takeJ" $ do
        let cases =
              [ (0 :: Int, Empty, [])
              , (1 :: Int, Empty, [])
              , -- single
                (0 :: Int, Single 1 "10", [])
              , (1 :: Int, Single 1 "10", ["10"])
              , (2 :: Int, Single 1 "10", ["10"])
              , -- append
                (0 :: Int, append, [])
              , (1 :: Int, append, ["1"])
              , (2 :: Int, append, ["1", "2"])
              , (3 :: Int, append, ["1", "2", "3"])
              , (4 :: Int, append, ["1", "2", "3", "4"])
              , (5 :: Int, append, ["1", "2", "3", "4"])
              ]
        forM_ cases $ \(i, tree, expected) ->
          it
            ( printf
                "takeJ n=%d list=%s, expected=%s"
                i
                (show (jlToList tree))
                (show expected)
            )
            $ jlToList (takeJ i tree) `shouldBe` expected
