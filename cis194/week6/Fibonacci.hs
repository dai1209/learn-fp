{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-
Translate the above definition of Fibonacci numbers directly into a
recursive function definition of type
fib :: Integer -> Integer
so that fib n computes the nth Fibonacci number Fn.
Now use fib to define the infinite list of all Fibonacci numbers,
fibs1 :: [Integer]
(Hint: You can write the list of all positive integers as [0..].)
Try evaluating fibs1 at the ghci prompt. You will probably get
bored watching it after the first 30 or so Fibonacci numbers, because
fib is ridiculously slow. Although it is a good way to define the Fibonacci numbers, it is not a very good way to compute them—in order
to compute Fn it essentially ends up adding 1 to itself Fn times! For
example, shown at right is the tree of recursive calls made by evaluating fib 5.

As you can see, it does a lot of repeated work. In the end, fib
has running time O(Fn), which (it turns out) is equivalent to O(ϕ
n
),
where ϕ =
1+
√
5
2
is the “golden ratio”. That’s right, the running time
is exponential in n. What’s more, all this work is also repeated from
each element of the list fibs1 to the next. Surely we can do better.
-}
fib :: Integer -> Integer

fibs1 :: [Integer]
fibs1 = map fib [0..]

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{-
When I said “we” in the previous sentence I actually meant “you”.
Your task for this exercise is to come up with more efficient implementation. Specifically, define the infinite list
fibs2 :: [Integer]
so that it has the same elements as fibs1, but computing the first n
elements of fibs2 requires only O(n) addition operations. Be sure to
use standard recursion pattern(s) from the Prelude as appropriate.
Of course there are several billion
Haskell implementations of the Fibonacci numbers on the web, and I have
no way to prevent you from looking
at them; but you’ll probably learn a
lot more if you try to come up with
something yourself first.
Streams
We can be more explicit about infinite lists by defining a type Stream
representing lists that must be infinite. (The usual list type represents
lists that may be infinite but may also have some finite length.)
In particular, streams are like lists but with only a “cons” constructor—
whereas the list type has two constructors, [] (the empty list) and
(:) (cons), there is no such thing as an empty stream. So a stream is
simply defined as an element followed by a stream.

• Define a data type of polymorphic streams, Stream.
• Write a function to convert a Stream to an infinite list,
streamToList :: Stream a -> [a]
• To test your Stream functions in the succeeding exercises, it will be
useful to have an instance of Show for Streams. However, if you put
deriving Show after your definition of Stream, as one usually does,
the resulting instance will try to print an entire Stream—which,
of course, will never finish. Instead, you should make your own
instance of Show for Stream,
instance Show a => Show (Stream a) where
show ...
which works by showing only some prefix of a stream (say, the
first 20 elements). 
-}
fibs2 :: [Integer]
fibs2 = [1,1] ++ [ fibs2!!(n - 2) + fibs2!!(n - 1) | n <- [2..]]

data Streams a = Cons a (Streams a)

streamToList :: Streams a -> [a]

streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Streams a) where
  show a = (init $ show $ take 40 $ streamToList a) ++ "..."


{-
Exercise 4
Let’s create some simple tools for working with Streams.
• Write a function
streamRepeat :: a -> Stream a
which generates a stream containing infinitely many copies of the
given element. 
• Write a function
streamMap :: (a -> b) -> Stream a -> Stream b
which applies a function to every element of a Stream.
• Write a function
streamFromSeed :: (a -> a) -> a -> Stream a
which generates a Stream from a “seed” of type a, which is the
first element of the stream, and an “unfolding rule” of type a -> a
which specifies how to transform the seed into a new seed, to be
used for generating the rest of the stream.
-}

streamRepeat :: a -> Streams a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Streams a -> Streams b
streamMap f (Cons a b) = Cons (f a) (streamMap f b) 

streamFromSeed :: (a -> a) -> a -> Streams a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

{-
Now that we have some tools for working with streams, let’s create a few:
• Define the stream
nats :: Stream Integer
which contains the infinite list of natural numbers 0, 1, 2, . . .
• Define the stream
ruler :: Stream Integer
which corresponds to the ruler function
0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
where the nth element in the stream (assuming the first element
corresponds to n = 1) is the largest power of 2 which evenly
divides n. Hint: define a function
interleaveStreams which alternates
the elements from two streams. Can
you use this function to implement
ruler in a clever way that does not have
to do any divisibility testing?
cis 194: homework 6 4
Fibonacci numbers via generating functions (extra credit)
This section is optional but very cool, so if you have time I hope you
will try it. We will use streams of Integers to compute the Fibonacci
numbers in an astounding way.
The essential idea is to work with generating functions of the form
a0 + a1x + a2x
2 + · · · + anx
n + . . .
where x is just a “formal parameter” (that is, we will never actually
substitute any values for x; we just use it as a placeholder) and all the
coefficients ai are integers. We will store the coefficients a0, a1, a2, . . .
in a Stream Integer.

-}
nats :: Streams Integer 
nats = streamFromSeed (+1) 0

-- ruler 
-- ruler :: Stream Integer

{-

-}
x :: Streams Integer

x = Cons 0 (Cons 1 $ streamRepeat 0) 

instance Num (Streams Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate (Cons a as) = Cons (-a) (negate as)
  (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
  (*) (Cons a as) bbs@(Cons b bs) = Cons (a * b) (streamMap (* a) bs + (as * bbs))

instance Fractional (Streams Integer) where
  (/) (Cons a as) (Cons b bs) = q 
    where q = Cons (a `div` b) (streamMap ( `div` b) (as - q * bs) ) -- 会递归代入q,惰性求值的原因,不会报错


fib3 :: Streams Integer
-- Streams Integer 代表x表达式各项的系数
fib3 = x / (1 - x - x^2)

-- fib4 :: Streams Integer

type Matrix = (Integer, Integer, Integer)

instance Num Matrix where
  (*) (x,y,z) (a,b,c) = (x*a + y*b, x*b + y*c, y*b + z * c)

m = (1,1,0)
-- fib4 = m 


