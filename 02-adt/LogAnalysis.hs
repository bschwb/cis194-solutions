{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Parses an individual line
-- > parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- > parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- > parseMessage "This is not in the right format"
-- >   == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in
                   case wordList of
                     ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
                     ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
                     ("E":lvl:ts:msg) -> LogMessage (Error (read lvl))
                                            (read ts) (unwords msg)
                     _ -> Unknown (unwords wordList)

-- Parses an entire log file at once and returns content as a list of
-- LogMessages.
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Assume a sorted MessageTree and produce new sorted MessageTree with
-- LogMessage inserted.
-- timestamps in the left subtree are less, timestamps in the right subtree are
-- greater than the timestamp in the LogMessage of the Node
-- If LogMessage is of type Unknown return unchanged MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert lmsg@(LogMessage _ _ _) Leaf = Node Leaf lmsg Leaf
insert lmsg1@(LogMessage _ ts1 _) (Node left (LogMessage _ ts2 _) right)
  | ts1 > ts2 = insert lmsg1 right
  | otherwise = insert lmsg1 left
insert _ tree = tree

