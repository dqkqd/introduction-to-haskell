module Homework2.LogAnalysis (parse, parseMessage, insert, build) where

import Homework2.Log
import Text.Read (readMaybe)

{- |
Build a complete MessageTree from a list of messages
-}
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

{- |
Insert a new LogMessage into an existing MessageTree.
-}
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg node@(Node left mid right) = case compareLogMessage msg mid of
  GT -> Node left mid (insert msg right)
  LT -> Node (insert msg left) mid right
  _ -> node

compareLogMessage :: LogMessage -> LogMessage -> Ordering
compareLogMessage (LogMessage _ lhsTs _) (LogMessage _ rhsTs _) = compare lhsTs rhsTs
compareLogMessage _ _ = EQ

{- |
Parse an individual messsage to a LogMessage.
-}
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : ts : rest) -> case parseTimeStamp ts of
    Just timestamp -> LogMessage Info timestamp (unwords rest)
    _ -> Unknown s
  ("W" : ts : rest) -> case parseTimeStamp ts of
    Just timestamp -> LogMessage Warning timestamp (unwords rest)
    _ -> Unknown s
  ("E" : lvl : ts : rest) -> case (parseInt lvl, parseTimeStamp ts) of
    (Just level, Just timestamp) -> LogMessage (Error level) timestamp (unwords rest)
    _ -> Unknown s
  _ -> Unknown s

{- |
Parse a log content file to a list of LogMessage.
-}
parse :: String -> [LogMessage]
parse content = map parseMessage (lines content)

parseTimeStamp :: String -> Maybe TimeStamp
parseTimeStamp = parseInt

parseInt :: String -> Maybe Int
parseInt = readMaybe
