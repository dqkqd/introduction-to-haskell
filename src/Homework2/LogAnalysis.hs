module Homework2.LogAnalysis (parse, parseMessage) where

import Homework2.Log
import Text.Read (readMaybe)

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
