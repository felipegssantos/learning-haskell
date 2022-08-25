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

