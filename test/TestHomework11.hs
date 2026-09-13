module TestHomework11 (spec) where

import Test.Hspec

import Control.Monad (forM_)
import Data.Char (isUpper)
import Homework11.AParser (
  runParser,
  satisfy,
 )
import Homework11.SExpr (ident, oneOrMore, spaces, zeroOrMore)

spec :: Spec
spec = do
  describe "Homework11" $ do
    describe "Exercise 1" $ do
      forM_
        [ ("ABCdEfgH", Just ("ABC", "dEfgH"))
        , ("abcdEfgH", Just ("", "abcdEfgH"))
        ]
        $ \(input, expected) ->
          it ("zeroOrMore" ++ input) $
            runParser (zeroOrMore (satisfy isUpper)) input `shouldBe` expected

      forM_
        [ ("ABCdEfgH", Just ("ABC", "dEfgH"))
        , ("abcdEfgH", Nothing)
        ]
        $ \(input, expected) ->
          it ("oneOrMore" ++ input) $
            runParser (oneOrMore (satisfy isUpper)) input `shouldBe` expected

    describe "Exercise 2" $ do
      forM_
        [ ("  abc", Just ("  ", "abc"))
        , ("abc", Just ("", "abc"))
        ]
        $ \(input, expected) ->
          it ("spaces" ++ input) $
            runParser spaces input `shouldBe` expected

      forM_
        [ ("foobar baz", Just ("foobar", " baz"))
        , ("foo33fA", Just ("foo33fA", ""))
        , ("2bad", Nothing)
        , ("", Nothing)
        ]
        $ \(input, expected) ->
          it ("ident" ++ input) $
            runParser ident input `shouldBe` expected
