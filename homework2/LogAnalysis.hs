{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
-- Returns message in the right format
formatMessage :: [String] -> String -> LogMessage
formatMessage ms h = LogMessage messageType (read (ms !! x))
                   (unwords (drop (x + 1) ms))
  where messageType
          | h == "I"  = Info
          | h == "W"  = Warning
          | otherwise = Error (read (ms !! 1))
        x
          | h == "E"  = 2
          | otherwise = 1

-- Parses an individual line from the log file
parseMessage :: String -> LogMessage
parseMessage m
  | h `elem` ["I", "W", "E"] = formatMessage ms h
  | otherwise                = Unknown "This is not in the right format"
  where
    ms = words m
    h = head ms

-- Parses a whole log file
parse :: String -> [LogMessage]
parse file = [parseMessage line | line <- lines file]
