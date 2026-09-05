module Homework1 (toDigits, toDigitsRev, doubleEveryOther, sumDigits, validate, hanoi) where

-- Exercise 1

{- | Find digits of a number.

>>> toDigits 1234
[1,2,3,4]
-}
toDigits :: Integer -> [Integer]
toDigits = rev . toDigitsRev

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
doubleEveryOther = rev . doubleEveryOtherLeftRight . rev

{- | Double every others from left to right.

>>> doubleEveryOtherLeftRight [1,2,3,4]
[1,4,3,8]
-}
doubleEveryOtherLeftRight :: [Integer] -> [Integer]
doubleEveryOtherLeftRight [] = []
doubleEveryOtherLeftRight [x] = [x]
doubleEveryOtherLeftRight (x1 : x2 : xs) = x1 : (x2 * 2) : doubleEveryOtherLeftRight xs

-- Exercise 3

{- | Sum all the digits.

>>> sumDigits [16,7,12,5]
22
-}
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = sumDigits xs + sum (toDigitsRev x)

-- Exercise 4

{- | Whether an Integer chould be a valid credit card number.

>>> validate 4012888888881881
True

>>> validate 4012888888881882
False
-}
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

{- |
The tower of hanoi. The move includes 3 steps:

* move n - 1 disc from a to c using b as temporary storage
* move the top disc from a to b
* move n - 1 disc from c to b using a as temporary storage

>>> hanoi 2 "a" "b" "c"
[("a","c"),("a","b"),("c","b")]
-}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a _ c = [(a, c)]
hanoi n a b c =
  -- 1. move n - 1 disc from a to c using b as temporary storage
  hanoi (n - 1) a b c
    ++
    -- 2. move the top disc from a to b
    [(a, b)]
    ++
    -- 3. move n - 1 disc from c to b using a as temporary storage
    hanoi (n - 1) c a b

{- | Reverse a list.

>>> rev [1,2,3,4]
[4,3,2,1]
-}
rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]
