### week1

```hs
{- 
练习1我们需要先找到数字的数字。定义

功能

toDigits:：Integer->[Integer]

toDigitsRev:：Integer->[Integer]

toDigits应该将正整数转换为数字列表(为0或

负输入，toDigits应返回空列表。）toDigitsRev

也应该这样做，但数字要颠倒。

示例：toDigits 1234==[1,2,3,4]

示例：toDigitsRev 1234==[4,3,2,1]

示例：toDigits 0==[]

示例：toDigits（-17）=[] -}

toDigits :: Integer -> [Integer]
toDigits n = if n <= 0 
             then []
             else map (\x -> read [x]) (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

{-
练习2一旦我们把数字按正确的顺序排列好，我们需要

每隔一个加倍。定义函数

doubleEveryOther:：[Integer]->[Integer]

记住doubleEveryOther应该从右边开始每隔一个数字加倍，即从第二个到最后一个，从第四个到最后一个，

. . . 人数增加了一倍。

示例：doubleEveryOther[8,7,6,5]==[16,7,12,5]

示例：doubleEveryOther[1,2,3]==[1,4,3]

 -}

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther = reverse . workFunc . reverse

workFunc [] = []
workFunc [a] = [a]
workFunc (a:b:ns) = a: (2*b) : (workFunc ns)


{-
练习3 Double Everyother的输出为一位数的混合

和两位数。定义函数

sumDigits:：[Integer]->Integer

计算所有数字之和。

示例：sumDigits[16,7,12,5]=1+6+7+1+2+5=22
-}

sumList [a] = a
sumList [a,b] = a + b


sunDigits :: [Integer] -> Integer
sunDigits = foldl (\a b ->  a + (sumList $ toDigits b)) 0


{-
练习4定义函数

验证：：整数->布尔

指示整数是否可以是有效的信用卡号。这将使用前面练习中定义的所有函数。

示例：验证40128888881881=True

示例：validate 40128888881882=False
-}


validate = (\x -> mod x 10 == 0) . sunDigits . doubleEveryOther . toDigits 



{- 
汉诺塔问题


-}

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

hanoi n a b c | n <= 0 = []
              | n == 1 = [(a,c)]
              | otherwise = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) b a c


{-
练习6（可选）如果有四个钉子而不是三个钉子怎么办？

也就是说，目标仍然是将一堆光盘从第一个钉移动到另一个钉

最后一个销钉，从未将较大的圆盘放在较小的圆盘上

一个，但现在有两个额外的钉子，可以作为“临时”存储，而不是只有一个。编写一个类似于hanoi的函数

它以尽可能少的动作解决了这个问题。

应该可以用比三个动作少得多的动作来完成

钉子。例如，三个钉子需要2^15 − 1=32767次移动

要传送15张光盘。有了四个钉子，它可以在129步中完成(看见

Graham、Knuth和Patashnik的练习1.17，具体数学，

第二版，Addison-Wesley，1994年）
-}

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d 
  | n <= 0 = []
  | n == 1 = [(a,d)]
  | n == 2 = [(a,b),(a,d),(b,d)]
  | n == 3 = [(a,b),(a,c),(a,d),(c,d),(b,d)]
  | otherwise = hanoi4 (n - 1) a b d c ++ hannoi4 1 a b c d ++ hannoi4 (n - 1) c a b d







```