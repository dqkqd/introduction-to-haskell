module TestHomework2 (spec) where

import Homework2.Log (
  LogMessage (LogMessage, Unknown),
  MessageType (Error, Info, Warning),
 )
import Homework2.LogAnalysis (parse, parseMessage)
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
