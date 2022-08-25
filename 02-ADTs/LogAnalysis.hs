{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parse a single log message
parseMessage :: String -> LogMessage
parseMessage = parseMessage' . words

-- actual message parser, works after message is broken into a list of strings
parseMessage' :: [String] -> LogMessage
parseMessage' ("E":severity:timestamp:rest) =
  LogMessage (Error (read severity)) (read timestamp) (unwords rest)
parseMessage' ("W":timestamp:rest) =
  LogMessage Warning (read timestamp) (unwords rest)
parseMessage' ("I":timestamp:rest) =
  LogMessage Info (read timestamp) (unwords rest)
parseMessage' other = Unknown (unwords other)

-- parse a log file
parse :: String -> [LogMessage]
parse logMessages = map parseMessage (lines logMessages)

-- insert a log message into an ordered binary search tree
insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ t _) (Node left msg'@(LogMessage _ t' _) right)
  | t < t' = Node (insert msg left) msg' right
  | t > t' = Node left msg' (insert msg right)
insert (Unknown _) tree = tree -- ignore unknown messages
-- when a Leaf is reached, replace it with a new node
insert msg Leaf = Node Leaf msg Leaf
-- evaluate to nonsensical value for unexpected inputs?
insert _ _ = (Node Leaf (Unknown "") Leaf)

-- build a sorted binary search tree from logs
build :: [LogMessage] -> MessageTree
build (msg:others) = insert msg (build others)
build [] = Leaf
