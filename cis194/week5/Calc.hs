{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M 

{-
Exercise 1
Write Version 1 of the calculator: an evaluator for ExprT, with the
signature
eval :: ExprT -> Integer
For example, eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.

-}

eval :: ExprT -> Integer

eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

{-
  The UI department has internalized the focus group data and is
  ready to synergize with you. They have developed the front-facing
  user-interface: a parser that handles the textual representation of the
  selected language. They have sent you the module Parser.hs, which
  exports parseExp, a parser for arithmetic expressions. If you pass
  the constructors of ExprT to it as arguments, it will convert Strings
  representing arithmetic expressions into values of type ExprT. For
  example:
  *Calc> parseExp Lit Add Mul "(2+3)*4"
  Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
  *Calc> parseExp Lit Add Mul "2+3*4"
  Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
  *Calc> parseExp Lit Add Mul "2+3*"
  Nothing
  Leverage the assets of the UI team to implement the value-added
  function
  evalStr :: String -> Maybe Integer
  which evaluates arithmetic expressions given as a String, producing Nothing for inputs which are not well-formed expressions, and
  Just n for well-formed inputs that evaluate to n.
-}

-- evalStr :: String -> Maybe Integer

-- evalStr = fmap eval . parseExp Lit Add Mul 

class Expr a where 
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where 
  mul = ExprT.Mul
  add = ExprT.Add
  lit = ExprT.Lit

reify :: ExprT -> ExprT
reify = id

{-Exercise 4
The marketing department has gotten wind of just how flexible
the calculator project is and has promised custom calculators to some
big clients. As you noticed after the initial roll-out, everyone loves the
interface, but everyone seems to have their own opinion on what the
semantics should be. Remember when we wrote ExprT and thought
that addition and multiplication of integers was pretty cut and dried?
Well, it turns out that some big clients want customized calculators
with behaviors that they have decided are right for them.
The point of our Expr type class is that we can now write down
arithmetic expressions once and have them interpreted in various
ways just by using them at various types.
cis 194: homework 5 4
Make instances of Expr for each of the following types:
• Integer — works like the original calculator
• Bool — every literal value less than or equal to 0 is interpreted as False, and all positive Integers
are interpreted as True; “addition” is logical or,
“multiplication” is logical and
• MinMax — “addition” is taken to be the max function, while
“multiplication” is the min function
• Mod7 — all values should be in the ranage 0 . . . 6, and
all arithmetic is done modulo 7; for example,
5 + 3 = 1.
The last two variants work with Integers internally, but in order
to provide different instances, we wrap those Integers in newtype
wrappers. These are used just like the data constructors we’ve seen
before.
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)
Once done, the following code should demonstrate our family of
calculators:
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
Try printing out each of those tests in ghci to see if things are
working. It’s great how easy it is for us to swap in new semantics for
the same syntactic expression!
-}

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where 
  lit = (> 0) 
  add = (||)
  mul = (&&)

instance Expr MinMax where 
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where 
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y) 

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp 

testBool :: Maybe Bool
testBool = testExp 
testMM :: Maybe MinMax
testMM = testExp 
testSat :: Maybe Mod7
testSat = testExp

{-The folks down in hardware have finished our new custom CPU,
so we’d like to target that from now on. The catch is that a stackbased architecture was chosen to save money. You need to write a
cis 194: homework 5 5
version of your calculator that will emit assembly language for the
new processor.
The hardware group has provided you with StackVM.hs, which
is a software simulation of the custom CPU. The CPU supports six
operations, as embodied in the StackExp data type:
data StackExp = PushI Integer
| PushB Bool
| Add
| Mul
| And
| Or
deriving Show
type Program = [StackExp]
PushI and PushB push values onto the top of the stack, which can
store both Integer and Bool values. Add, Mul, And, and Or each pop
the top two items off the top of the stack, perform the appropriate
operation, and push the result back onto the top of the stack. For
example, executing the program
[PushB True, PushI 3, PushI 6, Mul]
will result in a stack holding True on the bottom, and 18 on top of
that.
If there are not enough operands on top of the stack, or if an operation is performed on operands of the wrong type, the processor
will melt into a puddle of silicon goo. For a more precise specification of the capabilities and behavior of the custom CPU, consult the
reference implementation provided in StackVM.hs.
Your task is to implement a compiler for arithmetic expressions.
Simply create an instance of the Expr type class for Program, so that
arithmetic expressions can be interpreted as compiled programs. For
any arithmetic expression exp :: Expr a => a it should be the case
that
stackVM exp == Right [IVal exp]
Note that in order to make an instance for Program (which is a
type synonym) you will need to enable the TypeSynonymInstances
language extension, which you can do by adding
{-# LANGUAGE TypeSynonymInstances #-}
as the first line in your file.
Finally, put together the pieces you have to create a function
cis 194: homework 5 6
compile :: String -> Maybe Program
which takes Strings representing arithmetic expressions and compiles them into programs that can be run on the custom CPU.
-}

-- type Program = [StackExp]

compile :: String -> Maybe Program
compile = parseExp lit add mul

instance Expr Program where 
  lit x = [PushI x]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

-- "1+2+3" -> Maybe [PushI 1,Push 2,Add,Push 3, Add] 

{-
  Some users of your calculator have requested the ability to give
names to intermediate values and then reuse these stored values
later.
To enable this, you first need to give arithmetic expressions the
ability to contain variables. Create a new type class HasVars a which
contains a single method var :: String -> a. Thus, types which are
instances of HasVars have some notion of named variables.
Start out by creating a new data type VarExprT which is the same
as ExprT but with an extra constructor for variables. Make VarExprT
an instance of both Expr and HasVars. You should now be able to
write things like
*Calc> add (lit 3) (var "x") :: VarExprT
But we can’t stop there: we want to be able to interpret expressions containing variables, given a suitable mapping from variables
to values. For storing mappings from variables to values, you should
use the Data.Map module. Add
import qualified Data.Map as M
at the top of your file. The qualified import means that you must
prefix M. whenever you refer to things from Data.Map. This is standard practice, since Data.Map exports quite a few functions with
names that overlap with names from the Prelude. Consult the
Data.Map documentation to read about the operations that are supported on Maps. http://hackage.haskell.org/
packages/archive/containers/latest/
doc/html/Data-Map.html
Implement the following instances:
instance HasVars (M.Map String Integer -> Maybe Integer)
instance Expr (M.Map String Integer -> Maybe Integer)
The first instance says that variables can be interpreted as functions from a mapping of variables to Integer values to (possibly)
Integer values. It should work by looking up the variable in the
mapping.
The second instance says that these same functions can be interpreted as expressions (by passing along the mapping to subexpressions and combining results appropriately).
Note: to write these instances you will need to enable the FlexibleInstances
language extension by putting
cis 194: homework 5 7
{-# LANGUAGE FlexibleInstances #-}
as the first line in your file.
Once you have created these instances, you should be able to test
them as follows:
withVars :: [(String, Integer)]
-> (M.Map String Integer -> Maybe Integer)
-> Maybe Integer
withVars vs exp = exp $ M.fromList vs
*Calc> :t add (lit 3) (var "x")
add (lit 3) (var "x") :: (Expr a, HasVars a) => a
*Calc> withVars [("x", 6)] $ add (lit 3) (var "x")
Just 9
*Expr> withVars [("x", 6)] $ add (lit 3) (var "y")
Nothing
*Calc> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
Just 54
-}

-- 要让 var "x" 和 lit 3 属于同一类型 满足计算函数， 
-- 创造一个新的数据类型 VarExprT , 实现为 Expr 和 HasVars的实例， 就可以将表达式解析为 VarExprT 类型
-- 若将 M.Map String Integer -> Maybe Integer 实现为 Expr 和 HasVars的实例，表达式可解析为 M.Map String Integer -> Maybe Integer 
-- 将变量Map类型 传入，即可得到Maybe Integer , 实现为 Expr 的过程 就是设置 如何解析计算 此类型，具体类型不同，函数执行不同

class HasVars a where  -- 
  var:: String -> a 

data VarExprT = Var String 
              | Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
            deriving (Show,Eq) 

instance Expr VarExprT where 
  lit x = Calc.Lit x
  add a b = Calc.Add a b
  mul a b = Calc.Mul a b

instance HasVars VarExprT where 
  var a = Var a

instance HasVars (M.Map String Integer -> Maybe Integer) where
  -- var :: string -> (M.Map String Integer -> Maybe Integer)
  var = M.lookup 

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = (\_ -> Just a)
  add = assist (+)
  mul = assist (*)
  
assist opear f g = \x -> case f x of 
                    Nothing -> Nothing
                    Just a -> case g x of 
                                Nothing -> Nothing
                                Just b -> Just (opear a b)
  
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer 
withVars vs exp = exp $ M.fromList vs