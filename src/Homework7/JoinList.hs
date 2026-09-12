module Homework7.JoinList (
  JoinList (Empty, Single, Append),
  tag,
  (+++),
  indexJ,
  jlToList,
  dropJ,
) where

import Homework7.Sized (Sized (size), getSize)

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x y = Append (tag x <> tag y) x y

actualSize :: (Sized b) => b -> Int
actualSize = getSize . size

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- the empty list have no element
indexJ _ Empty = Nothing
indexJ n (Single sz x)
  -- the single list only contains one element
  | n == 0 && actualSize sz == 1 = Just x
  | otherwise = Nothing
indexJ n x@(Append _ lhs rhs)
  -- out of range
  | n >= actualSize (tag x) = Nothing
  -- only need to take the left
  | n < actualSize (tag lhs) = indexJ n lhs
  -- skip the left, take the index from the right
  | otherwise = indexJ (n - actualSize (tag rhs)) rhs

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ ::
  (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 x = x
-- we already cover the n = 0 case above, so this time n must be >= 1,
-- which give us the empty JoinList
dropJ _ (Single _ _) = Empty
dropJ n x@(Append _ lhs rhs)
  -- drop everything
  | n >= actualSize (tag x) = Empty
  -- only drop the left and take everything from the right
  | n < actualSize (tag lhs) = dropJ n lhs +++ rhs
  -- drop everything from the left and only take the right
  | otherwise = dropJ (n - actualSize (tag lhs)) rhs
