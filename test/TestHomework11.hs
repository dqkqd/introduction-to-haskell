{-# LANGUAGE QuasiQuotes #-}

module TestHomework11 (spec) where

import Text.RawString.QQ (r)

import Test.Hspec

import Control.Monad (forM_)
import Data.Char (isUpper)
import Homework11.AParser (
  runParser,
  satisfy,
 )
import Homework11.SExpr (ident, oneOrMore, parseSExpr, spaces, zeroOrMore)

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

    describe "Exercise 3" $ do
      forM_
        [ ("5", [r|Just (A (N 5),"")|])
        , ("foo3", [r|Just (A (I "foo3"),"")|])
        , ("(a)", [r|Just (Comb [A (I "a")],"")|])
        , ("(a 5)", [r|Just (Comb [A (I "a"),A (N 5)],"")|])
        ,
          ( "(bar (foo) 3 5 874)"
          , [r|Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],"")|]
          )
        ,
          ( "(((lambda x (lambda y (plus x y))) 3) 5)"
          , [r|Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],"")|]
          )
        ,
          ( "(  lots  of ( spaces in ) this ( one ) )"
          , [r|Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],"")|]
          )
        ]
        $ \(input, expected) ->
          it ("parseSExpr" ++ input) $
            show (runParser parseSExpr input) `shouldBe` expected
