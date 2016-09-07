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
insert lmsg@LogMessage{} Leaf = Node Leaf lmsg Leaf -- suggested by hlint
{-insert lmsg@(LogMessage _ _ _) Leaf = Node Leaf lmsg Leaf-}
insert lmsg1@(LogMessage _ ts1 _) (Node left lmsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left lmsg2 (insert lmsg1 right)
  | otherwise = Node (insert lmsg1 left) lmsg2 right
insert _ tree = tree

-- Build MessageTree from list of messages
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf -- pointfull style of this suggested by hlint
{-build [] = Leaf-}
{-build (lmsg:msgs) = insert lmsg (build msgs)-}

-- Return sorted list of LogMessages in sorted MessageTree (by timestamp)
-- from smallest to biggest.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lmsg right) = inOrder left ++ [lmsg] ++ inOrder right

-- Takes unsorted list of LogMessages and returns a list of the messages
-- corresponding to any errors with a severity of 50 or greater, sorted by
-- timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMessage . inOrder . build . filter (severe 50)

-- Returns True only when it's a Error with level > minLvl
severe :: Int -> LogMessage -> Bool
severe minLvl (LogMessage (Error lvl) _ _)
  | lvl > minLvl  = True
  | otherwise = False
severe _ _ = False

-- Strips extra information from LogMessages, leaves only list of Strings
extractMessage :: [LogMessage] -> [String]
extractMessage (LogMessage _ _ msg : msgs) = msg : extractMessage msgs
extractMessage _ = []

