module Homework6 (
  fibs1,
  fibs2,
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
  (+) (Cons l ls) (Cons r rs) = Cons (l + r) (ls + rs)

  -- [a0, a1, a2, a3, a4, ...]
  -- [b0, b1, b2, b3, b4, ...]
  --
  -- [b0a0, b0a1, b0a2, b0a3, b0a4, ...]
  -- [   0, b1a0, b1a1, b1a2, b1a3, ...]
  -- [   0,    0, b2a0, b2a1, b2a2, ...]
  -- [   0,    0,    0, b3a0, b3a1, ...]
  (*) (Cons l rest) rhs = mulStream l rhs + Cons 0 (rest * rhs)
   where
    mulStream :: Integer -> Stream Integer -> Stream Integer
    mulStream m (Cons y ys) = Cons (m * y) $ mulStream m ys
