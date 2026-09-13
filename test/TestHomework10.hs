module TestHomework10 (spec) where

import Test.Hspec

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM_)

import Homework10.AParser (
  Parser (Parser),
  abParser,
  abParser_,
  char,
  intPair,
  posInt,
  runParser,
 )

type Name = String
data Employee = Emp {name :: Name, phone :: String} deriving (Show, Eq)

firstAndRest :: String -> Maybe (String, String)
firstAndRest "" = Nothing
firstAndRest s = Just (w, dropWhile (== ' ') rest)
 where
  (w, rest) = span (/= ' ') s

spec :: Spec
spec = do
  describe "Homework10" $ do
    describe "Exercise 1" $ do
      it "Functors for Parser" $
        runParser ((+ 10) `fmap` posInt) "5A" `shouldBe` Just (15, "A")

    describe "Exercise 2" $ do
      let parseName = Parser firstAndRest
      let parsePhone = Parser firstAndRest
      let parseEmp = Emp <$> parseName <*> parsePhone :: Parser Employee

      it "Parser King 12345" $ do
        runParser parseEmp "King 12345"
          `shouldBe` Just (Emp{name = "King", phone = "12345"}, "")

      it "Parser King" $ do
        runParser parseEmp "King" `shouldBe` Nothing

      it "Parser ``" $ do
        runParser parseEmp "" `shouldBe` Nothing

    describe "Exercise 3" $ do
      forM_
        [ ("abcdef", Just (('a', 'b'), "cdef"))
        , ("aebcdf", Nothing)
        , ("", Nothing)
        ]
        $ \(input, expected) ->
          it ("abParser" ++ input) $
            runParser abParser input `shouldBe` expected

      forM_
        [ ("abcdef", Just ((), "cdef"))
        , ("aebcdf", Nothing)
        , ("", Nothing)
        ]
        $ \(input, expected) ->
          it ("abParser_" ++ input) $
            runParser abParser_ input `shouldBe` expected

      forM_
        [ ("12 34", Just ([12, 34], ""))
        , ("12 34 56", Just ([12, 34], " 56"))
        , ("12", Nothing)
        , ("", Nothing)
        ]
        $ \(input, expected) ->
          it ("intPair" ++ input) $
            runParser intPair input `shouldBe` expected

    describe "Exercise 4" $ do
      forM_
        [ ("ab", Just ('a', "b"))
        , ("ax", Just ('a', "x"))
        , ("bx", Just ('b', "x"))
        , ("x", Nothing)
        ]
        $ \(input, expected) ->
          it ("<|> " ++ input) $
            runParser (char 'a' <|> char 'b') input `shouldBe` expected
