{-# OPTIONS_GHC -Wno-missing-methods #-}

module Homework6 (
  fibs1,
  fibs2,
  fibs3,
  fib4,
  streamToList,
  streamRepeat,
  streamMap,
  streamFromSeed,
  Stream (Cons),
  nats,
  interleaveStreams,
  ruler,
  x,
) where

import Data.List (intercalate)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons c s) = c : streamToList s

instance (Show a) => Show (Stream a) where
  show s = "Stream[" ++ firstElements ++ ",...]"
   where
    firstElements = intercalate "," . map show . take 20 $ streamToList s

streamRepeat :: a -> Stream a
streamRepeat c = Cons c (streamRepeat c)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons c s) = Cons (f c) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f c = Cons c $ streamFromSeed f (f c)

nats :: Stream Integer
nats = streamFromSeed (+ (1 :: Integer)) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons c s) rhs = Cons c (interleaveStreams rhs s)

ruler :: Stream Integer
ruler = go 0
 where
  go :: Integer -> Stream Integer
  go n = interleaveStreams (streamRepeat n) (go (n + 1))

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate = streamMap (* (-1))
  (+) (Cons a0 a') (Cons b0 b') = Cons (a0 + b0) (a' + b')
  (*) (Cons a0 a') b = streamMap (* a0) b + Cons 0 (a' * b)

instance Fractional (Stream Integer) where
  (/) a@(Cons a0 a') b@(Cons b0 b') =
    Cons
      (a0 `div` b0)
      $ streamMap
        (`div` b0)
        (a' - (a / b) * b')

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

data Matrix = Matrix
  { a00 :: Integer
  , a01 :: Integer
  , a10 :: Integer
  , a11 :: Integer
  }
  deriving (Show, Eq)

instance Num Matrix where
  (*) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) =
    Matrix
      (a00 * b00 + a01 * b10)
      (a00 * b01 + a01 * b11)
      (a10 * b00 + a11 * b10)
      (a10 * b01 + a11 * b11)

fib4 :: Integer -> Integer
fib4 n = a01
 where
  Matrix _ a01 _ _ = Matrix 1 1 1 0 ^ n
