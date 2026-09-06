module Homework6 (fibs1, fibs2, streamToList, Stream (Cons)) where

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
streamToList (Cons x s) = x : streamToList s

instance (Show a) => Show (Stream a) where
  show s = show $ take 20 $ streamToList s
