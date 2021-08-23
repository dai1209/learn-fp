{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log
-- import Data.Char()

parseMessage :: String -> LogMessage

parseMessage s = let a:b:ss = words s
                 in case a of 
                    "I" -> LogMessage Info (read b) $ unwords ss  
                    "W" -> LogMessage Warning (read b) $ unwords ss  
                    "E" -> LogMessage (Error (read b)) (read $ head ss) $ unwords $ tail ss  
                    _   -> Unknown (unwords (a:b:ss))


parse :: String -> [LogMessage]
parse = map parseMessage . lines

data MessageTree = Leaf
  | Node MessageTree LogMessage MessageTree

insert :: LogMessage -> MessageTree -> MessageTree

insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert _ (Node _ (Unknown _) _) = Leaf
insert (LogMessage type1 time1 s1) (Node lTree (LogMessage type2 time2 s2) rTree) = 
  if time1 > time2 
  then Node lTree (LogMessage type2 time2 s2) (insert (LogMessage type1 time1 s1) rTree ) 
  else Node (insert (LogMessage type1 time1 s1) lTree ) (LogMessage type2 time2 s2) rTree

build :: [LogMessage] -> MessageTree

build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
-- 将树转成排序列表
inOrder Leaf = []

inOrder (Node lTree message rTree) = inOrder lTree ++ [message] ++ inOrder rTree

whatWentWrong :: [LogMessage] -> [String]

whatWentWrong = map getString . inOrder . build . filter isError50

isError50 :: LogMessage -> Bool
isError50 (Unknown _) = False
isError50 (LogMessage t _ _) =  
  case t of 
    Error a -> if a >= 50 then True else False
    _ -> False 


getString :: LogMessage -> String
getString (LogMessage _ _ string) = string
getString (Unknown string) = string


test :: IO ()
test = do 
  s <- readFile "./sample.log"
  print $ whatWentWrong $ parse s 

