module Homework1 (toDigits, toDigitsRev, doubleEveryOther, sumDigits) where

-- Exercise 1

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

-- Exercise 2

{- | Double every others from right to left.

>>> doubleEveryOther [1,2,3,4]
[2,2,6,4]
-}
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = rev (doubleEveryOtherLeftRight (rev n))

{- | Double every others from left to right.

>>> doubleEveryOtherLeftRight [1,2,3,4]
[1,4,3,8]
-}
doubleEveryOtherLeftRight :: [Integer] -> [Integer]
doubleEveryOtherLeftRight [] = []
doubleEveryOtherLeftRight [x] = [x]
doubleEveryOtherLeftRight (x1 : x2 : xs) = x1 : (x2 * 2) : (doubleEveryOtherLeftRight xs)

-- Exercise 3

{- | Sum all the digits.

>>> sumDigits [16,7,12,5]
22
-}
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = sumDigits xs + sum (toDigitsRev x)

{- | Reverse a list.

>>> rev [1,2,3,4]
[4,3,2,1]
-}
rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]
