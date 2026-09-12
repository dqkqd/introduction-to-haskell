module Homework7.JoinListBuffer (JoinListBuffer (JoinListBuffer)) where

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

newtype JoinListBuffer = JoinListBuffer (JoinList (Score, Size) String)
  deriving (Show, Eq)

inner :: JoinListBuffer -> JoinList (Score, Size) String
inner (JoinListBuffer b) = b

instance Buffer JoinListBuffer where
  toString b = toStringImpl $ inner b
   where
    toStringImpl x = case x of
      Empty -> ""
      (Single _ s) -> s
      (Append _ lhs rhs) -> toStringImpl lhs ++ "\n" ++ toStringImpl rhs

  fromString s = JoinListBuffer $ foldr ((+++) . scoreLine') Empty $ lines s

  line n = indexJ n . inner

  replaceLine n l b =
    JoinListBuffer $
      takeJ n (inner b) +++ scoreLine' l +++ dropJ (n + 1) (inner b)

  numLines b = getSize $ snd $ tag (inner b)

  value b = getScore $ fst $ tag (inner b)
