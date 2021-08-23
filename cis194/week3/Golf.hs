module Golf where

import Data.Map hiding (foldr, filter, map)

skips :: [a] -> [[a]]
{-  
  跳跃列表
  skips "ABCD" == ["ABCD", "BD", "C", "D"]
  skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
  skips [1] == [[1]]
  skips [True,False] == [[True,False], [False]]
  skips [] == []
-}
skips list = [ filterByIndex i list | i <- [1 .. length list]] 
filterByIndex 1 list = list
filterByIndex n list =map fst $ filter (\(_,i) -> i `mod` n == 0) $ zip list [1..]


localMaxima :: [Integer] -> [Integer]
{- 
  局部极大值 ： 严格大于前后2个值 （前后有值且大于）
  localMaxima [2,9,5,6,1] == [9,6]
  localMaxima [2,3,4,1,5] == [4]
  localMaxima [1,2,3,4,5] == []
-}

localMaxima [] = []
localMaxima [x] = []
localMaxima (x:y:[]) = []
localMaxima (a:b:c:xs) = if isMax a b c  then b : localMaxima (c:xs) else localMaxima (b:c:xs)

isMax a b c = if a < b && c < b then True else False



-- histogram :: [Integer] -> String
{- 

histogram [1,1,1,5] ==
*
*
* *
==========
0123456789
histogram [1,4,5,4,6,6,3,4,2,4,9] ==
*
*
* *
****** *
==========
0123456789

 -}

dataSouce = [(i,0) | i <- [0..9]]

histogram list = tail (parseMap $ toList $ getMap list) ++ axis 

getMap list =  foldr (adjust (+1)) (fromList  dataSouce) list 

parseMap list = let (res, list1) = foldr (\x y->  let (a,b) = x 
                                                      x1 = if b > 0 then "*" ++ fst y  else " " ++ fst y 
                                                      y1 = if b > 0 then  [(a,b-1)] ++ snd y else  [(a,b)] ++ snd y
                                                  in (x1, y1)) ("",[]) list 
                in if '*' `elem` res then parseMap list1 ++ "\n" ++ res else ""

axis = "\n==========\n0123456789\n"


test list = putStr $ histogram list