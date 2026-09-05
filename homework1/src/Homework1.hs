module Homework1 (toDigits, toDigitsRev) where

{- | Find digits of a number.

>>> toDigits 1234
[1,2,3,4]
-}
toDigits :: Integer -> [Integer]
toDigits n = rev (toDigitsRev n)

{- | Find reversed digit of a number.

>>> toDigitsRev 1234
[4,3,2,1]
-}
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

{- | Reverse a list.

>>> rev [1,2,3,4]
[4,3,2,1]
-}
rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]
