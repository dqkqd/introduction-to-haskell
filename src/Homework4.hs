module Homework4 (fun1, fun2, foldTree, Tree (Leaf, Node), xor, map') where

fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 =
  sum
    . filter even
    . takeWhile (> 1)
    . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

node :: Tree a -> a -> Tree a -> Tree a
node l m r = Node (1 + max (height l) (height r)) l m r

insert :: a -> Tree a -> Tree a
insert c Leaf = Node 0 Leaf c Leaf
insert c (Node _ lhs mid rhs)
  | height lhs <= height rhs = node (insert c lhs) mid rhs
  | otherwise = node lhs mid (insert c rhs)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
{- HLINT ignore "Use map" -}
map' f = foldr (\x acc -> f x : acc) []
