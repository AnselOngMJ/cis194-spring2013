{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
-- Parses an individual line from the log file
parseMessage :: String -> LogMessage
parseMessage m
  | validateFormat ms = formatMessage ms
  | otherwise = Unknown "This is not in the right format"
  where
    ms = words m

-- Returns message in the right format
formatMessage :: [String] -> LogMessage
formatMessage ms = LogMessage messageType (read (ms !! x))
                   (unwords (drop (x + 1) ms))
  where messageType
          | head ms == "I" = Info
          | head ms == "W" = Warning
          | otherwise      = Error (read (ms !! 1))
        x
          | head ms == "E" = 2
          | otherwise      = 1

-- Checks if a message is in the right format
validateFormat :: [String] -> Bool
validateFormat ms
  | head ms == "I" || head ms == "W" || head ms == "E" = True
  | otherwise                                          = False

-- Parses a whole log file
parse :: String -> [LogMessage]
parse file = [parseMessage line | line <- lines file]
