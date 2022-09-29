
-- for hLint Pragma
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} 

{-# HLINT ignore "Redundant section" #-}

-- ignoring type defaulting pick by GHC warning at this point
{-# OPTIONS_GHC -Wno-type-defaults #-}



-- `ModuleName () where` means export nothing, as opposed to `ModuleName where` which means expose everything.

module Learn where


{- Function declaration -}

half :: Fractional a => a -> a
half x = x / 2

square :: Num a => a -> a
square x =  x * x

triple :: Num a => a -> a
triple x = x * 3

{- Function call

  λ> half 6
  3.0
  λ> square 3
  9
  λ> triple 6
  18
-}


{-
  Write one function that has one parameter and works for all the following expressions.

  3.14 * (5 * 5)
  3.14 * (10 * 10)
  3.14 * (2 * 2)
  3.14 * (4 * 4)

-}
squareTimesPi :: Fractional a => a -> a
squareTimesPi x = 3.14 * x ^ 2

-- use pi from prelude instead
squareTimesPreludePi :: Floating a => a -> a
squareTimesPreludePi x = pi * x ^ 2

{-
  Functions in Haskell default to prefix syntax, meaning that the function being applied is at the beginning of the expression rather than the middle.
  While this is the default syntax for functions, not all functions are prefix.
  There are a group of operators, such as the arithmetic operators, that are indeed functions (they apply to arguments to produce an output) but appear by default in an infix position.
  Operators are functions that can be used in infix style. All operators are functions; not all functions are operators.

  Operator are function with a name that is a symbol and default to the infix syntax.
  Normal Function are function with a name that is alphanumeric and default to the prefix syntax.

  Normal function can be used in infix position provided they are surrounded by `` e.g. operand 'func' operand
  Operator can be used in prefix position provided they are surrounded by () e.g. (operator) operand operand

  In the example below please note that / and div are not equivalent function, as they operate on different type of Num

  :t (/)
  (/) :: Fractional a => a -> a -> a

  :t div
  div :: Integral a => a -> a -> a

-}

normalFunction :: Integer
normalFunction = div 10 4

normalFunctionInInfix :: Integer
normalFunctionInInfix = 10 `div` 4



operatorFunction :: Double
operatorFunction = 10 / 4

operatorFunctionInPrefix :: Double
operatorFunctionInPrefix = (/) 10 4




{- Associativity & Precedence

  Operator in haskell have associativity and precedence. GHCI can answer that for us.

  :i (*)
  infixl 7 *  -- left associative with precedence 7

  :i (+)
  infixl 6 +  -- left associative with precedence 6

  :i (^)
  infixr 8 ^  -- right associative with precedence 8

   1 + 2 ^ 3 = 1 + (2 ^ 3)

   1 - 2 + 3 = (1 - 2) + 3 = 2  ≠  1 - (2 + 3) = -4

-}
{- 
  Which parentizing  change the meaning of the expression

  1.
    a) 8 + 7 * 9
    b) (8 + 7) * 9

  ==> a) ≠ b)

  2.
    a) perimeter x y = (x * 2) + (y * 2)
    b) perimeter x y = x * 2 + y * 2

  ==> a) = b)

  3.
    a) f x = x / 2 + 9
    b) f x = x / (2 + 9)

  ==> a) ≠ b)

-}

{- Declaring Values

  The order of declarations in a source code file doesn’t matter, because GHCi (or GHC for the matter) loads the entire file at once, so it knows all the values that have been defined.

  On the other hand, when you enter them one by one into the REPL, the order does matter.

-}

valDecl1 :: Integer
valDecl1 = 1

valDecl2 :: Integer
valDecl2 = 2


{- Indentation rules


  In source code files, indentation often replaces syntactic markers like curly braces, semicolons, and parentheses. 
  
  (1) The basic rule is that code that is part of an expression should be indented under the beginning of that expression, even when the beginning of the expression is not at the leftmost margin.
   
  (2) Furthermore, parts of the expression that are grouped should be indented to the same level.

  (3) all declarations in the module must start at the same column. The column that all declarations within a module must start in is determined by the first declaration in the module.

  (4) Whitespace is often the only mark of a function call, unless parentheses are necessary due to conflicting precedence. Trailing whitespace, that is, extraneous whitespace at the end of a line of code, is considered bad style.

   
-}

{- 
  (1) -- need to be in a top level declaration
    let 
      x = 3
      y = 2

  (2) -- need to be in a top level declaration
    let x = 3 
        y = 4
  (3)
    foo x = 
      let y = x * 2 --group--
          z = x ^ 2 
      in 2 * y * z

  (4)
    x = 10
        * 5 + 4
-}

foo :: Num a => a -> a
foo x =
      let y = x * 2 
          z = x ^ 2
      in 2 * y * z

xDecl :: Integer
xDecl = 10
    * 5 + 4


{- Arithmetic Functions

  + plus addition
  - minus subtraction
  * asterisk multiplication
  / slash fractional division
  div divide integral division, round down
  quot quotient integral division, round toward zero
  mod modulo like rem, but after modular division
  rem remainder remainder after division

mod and rem are very subtle to use:
a key difference here is that, in Haskell (not in all languages), if one or both arguments are negative, the results of mod will have the same sign as the divisor, while the result of rem will have the same sign as the dividend:
Figuring out when you need mod takes some experience.
(Page 51).

-}

{- Negative numbers

 Due to the interaction of parentheses, currying, and infix syntax, negative numbers get special treatment in Haskell.

 To get a value that is a negative number by itself, adding a minus in front works fine:
 λ> -1000
 -1000

 However "obviously "in an expression it does not work
 λ> 1000 + -9
 <interactive>:20:1: error:
     Precedence parsing error
         cannot mix ‘+’ [infixl 6] and prefix `-' [infixl 6] in the same infix expression

 This works
 λ> 1000 + (-9)
 991

 The negation of numbers in Haskell by the use of a unary - sign is a form of syntactic sugar.

 In the specific case of -, the syntactic sugar means the operator now has two possible interpretations.
 The two possible interpretations of the syntactic - are either that - is being used as an alias for negate or that it is the subtraction function.

 -- negation
 λ> 2000 + (-1234)
 766
 λ> 2000 + (negate 1234)
 766

 -- subtraction
 λ> 2000 - 1234
 766
 
-}

{- $ a.k.a Dollar 

  λ> :i ($)
  
  ($) :: (a -> b) -> a -> b       -- Defined in ‘GHC.Base’
  
  infixr 0 $ -- means it is right associative and have the lowest precedence
  
  $ :: (a -> b) -> a -> b can be re-written as f $ a = f a where f :: a -> b

  $ has the lowest precedence i.e. 0, and is right-associative, so it allows
  parentheses to be omitted:

  f $ g $ h x  =  f (g (h x))

  Note that (h x) is ordinary function application and in haskell it has the highest precedence i.e. 10
  https://stackoverflow.com/questions/71031388/precedence-of-function-application
  Hence it is parented first.

  The $ function evaluates everything to its right first and can thereby be used to delay function application.

  Generally because of the lowest priority, operands of $  (left or right) are evaluated before the $

  λ> (2^) 2 + 2
  6
  λ> (2^) (2 + 2)
  16
  λ> (2^) $ 2 + 2
  16

  (2^) $ 2 + 2 $ (*30) -- this is wrong it comes, as it would fail at 4 (*30), where it try to apply 4 to (*30). 4hoowever is not a function.



-}

dollarSign :: Integer
dollarSign = (3^) $ 2 + 2 -- 3^(2 + 2)

dollarSign' :: Integer
dollarSign' = (3^) 2 + 2  -- (3^2) + 2




{- Sectioning

  If the infix function is >>, then we must write (>>) to refer to it as a value. It makes it usable a prefix function.

    Operator can be used in prefix position provided they are surrounded by () e.g. (operator) operand operand
    Normal function can be used in infix position provided they are surrounded by `` e.g. operand 'func' operand


  We can partially apply the function and this is called sectioning. We can section Operator from either side..

  (^2) -- which has type  Num a => a -> a

  (2^) -- which has type  (Integral b, Num a) => b -> a


  Subtraction, -, is a special case.

  These will work:

  λ> 2 - 1    -- because it is unambiguously subtraction
  1
  λ> (-) 2 1  -- because it is unambiguously subtraction
  1

  The following, however, won’t work:

  λ> (-2) 1

  Enclosing a value inside the parentheses with the '-' operator indicates to GHCi that it’s the argument of a function.
  Because the '-' function represents negation, not subtraction, when it’s applied to a single argument, GHCi does not know what to do with it, and so it returns an error message.
  Here, '-' is a case of syntactic overloading disambiguated by how it is used.

  We can use sectioning for subtraction, but it must be the first argument:
  λ> x = 5
  λ> y = (1-)
  λ> y x
  -4

  Or, instead of (- x), we can write (subtract x):

  λ> (subtract 2) 3
  1


-}

plus3 :: Integer -> Integer
plus3 = (+3) -- Sectioning

fourPlus3 :: Integer
fourPlus3 = plus3 4 -- Sectioned operator applied

--To refer to an operator on its own i.e. as a function i.e. value

plus :: Integer -> Integer -> Integer
plus = (+)

substract2 :: Integer -> Integer
substract2 = (subtract 2) --no need for  parentheses in this case as we are using a normal function, so a simple partial application is ok subtract 2

substract2From3 :: Integer
substract2From3 = substract2 3