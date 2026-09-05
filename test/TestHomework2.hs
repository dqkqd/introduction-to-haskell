module TestHomework2 (spec) where

import Homework2.Log (
  LogMessage (LogMessage, Unknown),
  MessageTree (Leaf, Node),
  MessageType (Error, Info, Warning),
 )
import Homework2.LogAnalysis (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
 )
import Test.Hspec

spec :: Spec
spec = do
  describe "Exercise 1" $ do
    it "parseMessage I 29 la la la" $
      parseMessage "I 29 la la la"
        `shouldBe` LogMessage Info 29 "la la la"

    it "parseMessage W 29 la la la" $
      parseMessage "W 29 la la la"
        `shouldBe` LogMessage Warning 29 "la la la"

    it "parseMessage E 2 562 help help" $
      parseMessage "E 2 562 help help"
        `shouldBe` LogMessage (Error 2) 562 "help help"

    it "parseMessage This is not the right format" $
      parseMessage "This is not the right format"
        `shouldBe` Unknown "This is not the right format"

    it "parse" $
      parse
        ( unlines
            [ "I 29 la la la"
            , "W 29 la la la"
            , "E 2 562 help help"
            , "This is not the right format"
            ]
        )
        `shouldBe` [ LogMessage Info 29 "la la la"
                   , LogMessage Warning 29 "la la la"
                   , LogMessage (Error 2) 562 "help help"
                   , Unknown "This is not the right format"
                   ]

  describe "Exercise 2" $ do
    let tree = Node Leaf (LogMessage Info 10 "ten") Leaf

    it "insert into empty tree wraps the message" $
      insert (LogMessage Info 1 "one") Leaf
        `shouldBe` Node Leaf (LogMessage Info 1 "one") Leaf

    it "insert Unknown leaves the tree unchanged" $
      insert (Unknown "garbage") tree `shouldBe` tree

    it "insert with smaller timestamp goes left" $
      insert (LogMessage Info 5 "five") tree
        `shouldBe` Node
          (Node Leaf (LogMessage Info 5 "five") Leaf)
          (LogMessage Info 10 "ten")
          Leaf

    it "insert with greater timestamp goes right" $
      insert (LogMessage Info 12 "twelve") tree
        `shouldBe` Node
          Leaf
          (LogMessage Info 10 "ten")
          (Node Leaf (LogMessage Info 12 "twelve") Leaf)

    it "insert with equal timestamp leaves the tree unchanged" $
      insert (LogMessage Info 10 "duplicate") tree `shouldBe` tree

  describe "Exercise 3" $ do
    it "build" $
      build
        [ LogMessage Info 5 "five2"
        , LogMessage Info 3 "three"
        , LogMessage Info 4 "four"
        , LogMessage Info 2 "two"
        , Unknown "garbage"
        , LogMessage Info 1 "one"
        , LogMessage Info 5 "five"
        , Unknown "garbage"
        ]
        `shouldBe` Node
          ( Node
              Leaf
              (LogMessage Info 1 "one")
              ( Node
                  Leaf
                  (LogMessage Info 2 "two")
                  ( Node
                      (Node Leaf (LogMessage Info 3 "three") Leaf)
                      (LogMessage Info 4 "four")
                      Leaf
                  )
              )
          )
          (LogMessage Info 5 "five")
          Leaf

  describe "Exercise 4" $ do
    it "inOrder" $
      inOrder
        ( build
            [ LogMessage Info 5 "five2"
            , LogMessage Info 3 "three"
            , LogMessage Info 4 "four"
            , LogMessage Info 2 "two"
            , Unknown "garbage"
            , LogMessage Info 1 "one"
            , LogMessage Info 5 "five"
            , Unknown "garbage"
            ]
        )
        `shouldBe` [ LogMessage Info 1 "one"
                   , LogMessage Info 2 "two"
                   , LogMessage Info 3 "three"
                   , LogMessage Info 4 "four"
                   , LogMessage Info 5 "five"
                   ]

  describe "Exercise 5" $ do
    it "whatWentWrong" $
      whatWentWrong
        ( parse
            ( unlines
                [ "I 6 Completed armadillo processing"
                , "I 1 Nothing to report"
                , "E 99 10 Flange failed!"
                , "I 4 Everything normal"
                , "I 11 Initiating self-destruct sequence"
                , "E 70 3 Way too many pickles"
                , "E 65 8 Bad pickle-flange interaction detected"
                , "W 5 Flange is due for a check-up"
                , "I 7 Out for lunch, back in two time steps"
                , "E 20 2 Too many pickles"
                , "I 9 Back from lunch"
                ]
            )
        )
        `shouldBe` [ "Way too many pickles"
                   , "Bad pickle-flange interaction detected"
                   , "Flange failed!"
                   ]
