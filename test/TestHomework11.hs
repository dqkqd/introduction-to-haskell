module TestHomework11 (spec) where

import Test.Hspec

import Control.Monad (forM_)
import Data.Char (isUpper)
import Homework11.AParser (
  runParser,
  satisfy,
 )
import Homework11.SExpr (oneOrMore, zeroOrMore)

type Name = String
data Employee = Emp {name :: Name, phone :: String} deriving (Show, Eq)

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
