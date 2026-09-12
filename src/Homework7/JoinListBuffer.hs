module Homework7.JoinListBuffer where

import Homework7.Buffer
import Homework7.JoinList (
  JoinList (Append, Empty, Single),
  dropJ,
  indexJ,
  tag,
  takeJ,
  (+++),
 )
import Homework7.Scrabble (Score, getScore, scoreLine')
import Homework7.Sized (Size, getSize)

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ lhs rhs) = toString lhs ++ "\n" ++ toString rhs

  fromString s = foldr ((+++) . scoreLine') Empty $ lines s

  line = indexJ

  replaceLine n l b = takeJ n b +++ scoreLine' l +++ dropJ (n + 1) b

  numLines b = getSize $ snd $ tag b

  value b = getScore $ fst $ tag b
