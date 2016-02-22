module LogAnalysis where

import Control.Applicative
import Log

parseInfo :: [String] -> LogMessage
parseInfo ws = LogMessage Info ts msg
    where ts = read . head . take 1 . drop 1 $ ws
          msg = unwords . drop 2 $ ws

parseWarning :: [String] -> LogMessage
parseWarning ws = LogMessage Warning ts msg
    where ts = read . head . take 1 . drop 1 $ ws
          msg = unwords . drop 2 $ ws

parseError :: [String] -> LogMessage
parseError ws = LogMessage (Error e) ts msg
    where e = read . head . take 1 . drop 1 $ ws
          ts = read . head . take 1 . drop 2 $ ws
          msg = unwords . drop 3 $ ws

parseMessage :: String -> LogMessage
parseMessage m = case take 1 m of
                     "E" -> parseError ws
                     "I" -> parseInfo ws
                     "W" -> parseWarning ws
                     _ -> Unknown m
    where ws = words m

getTimestamp :: LogMessage -> TimeStamp
getTimestamp (LogMessage _ ts _) = ts

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ s) = s

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left node right)
    | getTimestamp msg <= getTimestamp node = Node (insert msg left) node right
    | getTimestamp msg > getTimestamp node = Node left node (insert msg right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

isErrorOver50 :: LogMessage -> Bool
isErrorOver50 (LogMessage (Error s) _ _) = s > 50
isErrorOver50 _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = map getMessage $ filter isErrorOver50 ms
