import Data.List
import Data.Ord

-- 给你两个 非空 的链表，表示两个非负的整数。它们每位数字都是按照 逆序 的方式存储的，并且每个节点只能存储 一位 数字。
-- 请你将两个数相加，并以相同形式返回一个表示和的链表。
-- 你可以假设除了数字 0 之外，这两个数都不会以 0 开头。

add x y = if (x + y) >= 10
          then (1, x + y - 10)
          else (0,x + y)

l1 = [2,4,3]
l2 = [5,6,4]

addNumber :: (Ord a, Num a) => [a] -> [a] -> [a]
addNumber [] b = b
addNumber a [] = a
addNumber (a:as) (b:bs) = let (x, y) = add a b in
                          if x == 1
                          then y : (addNumber1 as bs)
                          else y : (addNumber as bs)

addNumber1 :: (Ord a, Num a) => [a] -> [a] -> [a]
addNumber1 [] bs = addNumber [1] bs
addNumber1 as [] = addNumber as [1]
addNumber1 (a:as) (b:bs) = let (x,y) = add (a+1) b in
                           if x == 1
                           then y : (addNumber1 as bs)
                           else y : (addNumber as bs)


-- 给定一个字符串 s ，请你找出其中不含有重复字符的 最长子串 的长度。

getHeadStr :: [a] -> [[a]]
getHeadStr x = [ take a x | a <- [1 .. length x] ]

getSubStr :: [a] -> [[a]]
getSubStr x = concat [getHeadStr $ drop a x | a <- [0 .. length x - 1] ]

substring :: [a] -> [[a]]
-- (=<<) :: Monad m => (a -> m b) -> m a -> m b
substring s = tail . inits =<< tails s


hasSameChar :: Eq a => [a] -> Int
hasSameChar s = let f = find (\ x -> length (elemIndices x s) > 1) s in 
                case f of
                  Just a -> 0
                  Nothing -> length s

maxNum :: (Num a ,Ord a) => [a] -> a
maxNum [] = 0
maxNum [x] = x
maxNum (x:xs) = max x $ maxNum xs

findMaxLen :: Eq a => [a] -> Int
findMaxLen = maxNum . (map hasSameChar) . getSubStr

-- 给定两个大小分别为 m 和 n 的正序（从小到大）数组 nums1 和 nums2。请你找出并返回这两个正序数组的 中位数 。

-- substring s = tail . inits =<< tails s
findPalindrome :: Ord a => [a] -> [a]
findPalindrome = head . sortBy (\ a b -> compare (length b) (length a)) . filter (\ x -> x == reverse x) . substring


-- 将一个给定字符串 s 根据给定的行数 numRows ，以从上往下、从左到右进行 Z 字形排列。
-- 之后，你的输出需要从左往右逐行读取，产生出一个新的字符串，比如："PAHNAPLSIIGYIR"。
-- 请你实现这个将字符串进行指定行数变换的函数：

createList :: (Num a1, Enum a1) => a1 -> [[a2]]
createList n = map (\_ -> []) [1 .. n]

indexList :: (Foldable t, Num a1, Enum a1) => t a2 -> a1 -> [a1]
indexList s n = take (length s) $ cycle ([0..n-1] ++ reverse [1..n-2])

-- x 为初始值 y 为遍历对象 fn 计算新的x值
fn :: [[a]] -> (a, Int) -> [[a]]
fn x (s, i) = let (a,b:bs) = splitAt i x
                  newb = b ++ [s]
              in a ++ (newb:bs)

convert :: [a] -> Int -> [a]
convert s n = let indexs = indexList s n
                  list = createList n
              in concat $ foldl fn list $ zip s indexs


-- 给你一个 32 位的有符号整数 x ，返回将 x 中的数字部分反转后的结果。
-- 如果反转后整数超过 32 位的有符号整数的范围 [−231,  231 − 1] ，就返回 0。
myAtoi :: Int -> Int
myAtoi num = let s = show num in
             if num < 0 
             then let (a:as) = s in read $ a : reverse as 
             else read $ reverse s


{-- 
  给你一个字符串 s 和一个字符规律 p，请你来实现一个支持 '.' 和 '*' 的正则表达式匹配。

  '.' 匹配任意单个字符
  '*' 匹配零个或多个前面的那一个元素
  所谓匹配，是要涵盖 整个 字符串 s的，而不是部分字符串。
--}
-- 扫描解析p 找出*和其之前一位的组合
forP :: String -> [String] -> [String]
forP [] l = l
forP [a] l = l ++ [a:[]]
forP (a:b:ps) l = if b == '*'
                  then forP ps (l ++ [a:b:[]]) 
                  else forP (b:ps) (l ++ [a:[]])

match :: String -> [String] -> Bool
match [] [] = True
match (s:ss) [] = False
match [] (p:ps) = case p of 
                  [a,b] -> match [] ps 
                  [a]   -> False
match (s:ss) (p:ps) = case p of
                      [a,b] -> if(s == a || a == '.') 
                               then match ss (p:ps)
                               else match (s:ss) ps
                      [a]   -> if (s == a || a == '.') 
                               then match ss ps
                               else False

isMatch :: String -> String -> Bool
isMatch s p = match s $ forP p []

{- 
  给你 n 个非负整数 a1，a2，...，an，每个数代表坐标中的一个点 (i, ai) 。在坐标内画 n 条垂直线，垂直线 i 的两个端点分别为 (i, ai) 和 (i, 0) 。找出其中的两条线，使得它们与 x 轴共同构成的容器可以容纳最多的水。
  说明：你不能倾斜容器。
-}

getArea :: (Num a, Ord a) => (a, a) -> (a, a) -> a 
getArea (a,i) (b,j) = min a b * (j - i)

maxArea :: (Num a, Ord a, Enum a) => [a] -> a
movePoint [x] m = m
movePoint ls m = let (a,i) = head ls 
                     (b,j) = last ls 
                     n = getArea (a,i) (b,j)
                 in  if m > n
                     then if a < b
                          then movePoint (tail ls) m
                          else movePoint (init ls) m
                     else if a < b
                          then movePoint (tail ls) n
                          else movePoint (init ls) n

maxArea ls = movePoint (zip ls [0..]) 0

{-
  罗马数字包含以下七种字符： I， V， X， L，C，D 和 M。
  整数转罗马字符
  提示：
    1 <= num <= 3999
-}

zipNum n = zip (show n) (reverse [1 .. length $ show n])

toRoman :: (Char, Int) -> String
toRoman (a, i) = case i of 
                 4 ->   replicate b 'M'
                 3 ->   toRoman3 b
                 2 ->   toRoman2 b
                 1 ->   toRoman1 b      
              where 
                b = read [a] :: Int

toRoman1 :: Int -> String
toRoman2 :: Int -> String
toRoman3 :: Int -> String
toRoman1 b  | b == 9 = "IX"
            | b == 4 = "IV"
            | b >= 5 = 'V': replicate (b - 5) 'I'
            | b < 4  = replicate b 'I'  

toRoman2 b  | b == 9 = "XC"
            | b == 4 = "XL"
            | b >= 5 = 'L': replicate (b - 5) 'X'
            | b < 4  = replicate b 'X'            

toRoman3 b  | b == 9 = "CM"
            | b == 4 = "CD"
            | b >= 5 = 'D': replicate (b - 5) 'C'
            | b < 4  = replicate b 'C'

intToRoman :: Int -> String
intToRoman = concat . (map toRoman) . zipNum 

-- 编写一个函数来查找字符串数组中的最长公共前缀。
commonPrefix :: String -> String -> String
commonPrefix as [] = ""
commonPrefix [] bs = ""
commonPrefix (a:as) (b:bs) = if a == b 
                             then a: commonPrefix as bs
                             else ""

longestCommonPrefix :: [String] -> String
longestCommonPrefix = foldl1 commonPrefix

{-
  给你一个包含 n 个整数的数组 nums，判断 nums 中是否存在三个元素 a，b，c ，使得 a + b + c = 0 ？请你找出所有和为 0 且不重复的三元组。
  注意：答案中不可以包含重复的三元组。
-}
-- 先排序， 




