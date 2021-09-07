{-
  fun1 :: [Integer] -> Integer
  fun1 [] = 1
  fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs
  2. fun2 :: Integer -> Integer
  fun2 1 = 0
  fun2 n | even n = n + fun2 (n ‘div‘ 2)
  | otherwise = fun2 (3 * n + 1)
  Hint: For this problem you may wish to use the functions iterate
  and takeWhile. Look them up in the Prelude documentation to see
  what they do.
-}

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
      
fun1' :: [Integer] -> Integer
fun1' = product 
      . map (\x -> x - 2) 
      . filter even

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/=1)
      . iterate f 

f x = if even x then x `div` 2 else 3 * x + 1

{- 
data Tree a = Leaf
| Node Integer (Tree a) a (Tree a)
deriving (Show, Eq)
For this exercise, write a function
cis 194: homework 4 2
foldTree :: [a] -> Tree a
foldTree = ...
which generates a balanced binary tree from a list of values using
foldr.
For example, one sample output might be the following, also visualized at right:
D
E
A
G
H
B
F C
I
J
foldTree "ABCDEFGHIJ" ==
Node 3
(Node 2
(Node 0 Leaf ’F’ Leaf)
’I’
(Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
’J’
(Node 2
(Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
’H’
(Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))
Your solution might not place the nodes in the same exact order,
but it should result in balanced trees, with each subtree having a
correct computed height.
 -}
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)
-- 平衡2叉树 节点值为 此树的高 左子树 节点 右子树
foldTree :: [a] -> Tree a 


foldTree [] = Leaf

foldTree xs = foldr insertTree Leaf xs
getH :: Tree a -> Integer
getH Leaf = -1
getH (Node _ l x r) = 1 + max (getH l) (getH r)

insertTree :: a -> Tree a -> Tree a
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node n l x1 r) | getH l >= getH r = let ntree = insertTree x r in Node ((getH ntree) + 1) l x1 ntree
                             | otherwise        = Node ((getH r) + 1) (insertTree x l) x1 r

{-1. Implement a function
xor :: [Bool] -> Bool
which returns True if and only if there are an odd number of True
values contained in the input list. It does not matter how many
False values the input list contains. For example,
xor [False, True, False] == True
xor [False, True, False, False, True] == False
Your solution must be implemented using a fold.-}
xor :: [Bool] -> Bool
xor = foldr1 (/=)

{-2. Implement map as a fold. That is, complete the definition
map’ :: (a -> b) -> [a] -> [b]
map’ f = foldr ...
in such a way that map’ behaves identically to the standard map
function.-}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> (f a):b) []

{-3. (Optional) Implement foldl using foldr. That is, complete the
definition
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr ...
in such a way that myFoldl behaves identically to the standard
foldl function.
Hint: Study how the application of foldr and foldl work out:
foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn-}

myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl f base xs = foldr (\x g b -> g $ f b x) id xs base    -- 使用foldr 创建一个函数， 通过 将 遍历对象的每一项作为 参数 构造未完成的闭包函数  foldr (\ b a -> ) base xs

{-
  Exercise 4: Finding primes
  Read about the Sieve of Sundaram. Implement the algorithm us- http://en.wikipedia.org/wiki/Sieve_
  of_Sundaram ing function composition. Given an integer n, your function should
  generate all the odd prime numbers up to 2n + 2.
  sieveSundaram :: Integer -> [Integer]
  sieveSundaram = ...
  To give you some help, below is a function to compute the Cartesian product of two lists. This is similar to zip, but it produces all
  possible pairs instead of matching up the list elements. For example,
  cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
  It’s written using a list comprehension, which we haven’t talked about
  in class (but feel free to research them).
  cartProd :: [a] -> [b] -> [(a, b)]
  cartProd xs ys = [(x,y) | x <- xs, y <- ys]
-}

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = tail $ myFilter [2..2*n+2]
myFilter :: [Integer] -> [Integer]
myFilter [] = []
myFilter (x:xs) = x: myFilter list
  where
    list = filter (\a -> (a `mod` x) /= 0) xs