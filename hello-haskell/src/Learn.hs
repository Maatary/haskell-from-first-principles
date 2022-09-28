{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant section" #-}
module Learn where -- () this means export nothing

{- Function declaration -}
half x = x / 2

square x =  x * x

triple x = x * 3


{-
  Write one function that has one parameter and works for all the following expressions.
  3.14 * (5 * 5)
  3.14 * (10 * 10)
  3.14 * (2 * 2)
  3.14 * (4 * 4)

-}
squareTimesPi x = 3.14 * x ^ 2

-- use pi from prelude instead
squareTimesPreludePi x = pi * x ^ 2

{-
  Operator are function with infix semantic.
  Normal function i.e. which start with alpha-numeric, have prefix semantic.

  Normal function can be used in infix position provided they are surrounded by `` e.g. operand 'fun' operand
  Operator can be used in prefix position provided they are surrounded by () e.g. (operator) operand operand

  In the example below please note that / and div are not equivalent function, as they operate on different type of Num

  :t (/)
  (/) :: Fractional a => a -> a -> a

  :t div
  div :: Integral a => a -> a -> a

-}

normalFunction = div 10 4
normalFunctionInInfix = 10 `div` 4

operatorfunction = 10 / 4
operatorfunctionInPrefix = (/) 10 4


{- Precedence -}

{- 

  perimeter' = perimeter
  
  Because (*) has precedence over (+)

  :i (*)
  infixl 7 *

  :i (+)
  infixl 6 +


-}

perimeter x y = (x * 2) + (y * 2)

perimeter' x y =  x * 2 + y * 2  


{- Indentation rules -}

{-
  In source code files, indentation often replaces syntactic markers like curly braces, semicolons, and parentheses. 
  
  (1) The basic rule is that code that is part of an expression should be indented under the beginning of that expression, even when the beginning of the expression is not at the leftmost margin.
   
  (2) Furthermore, parts of the expression that are grouped should be indented to the same level.
   
-}

{- 
  (1)
    let 
      x = 3
      y = 2
  (2)
    let x = 3 
        y = 4
  (2)
    foo x = 
      let y = x * 2 --group--
          z = x ^ 2 
    in 2 * y * z
-}

foo x = 
      let y = x * 2 
          z = x ^ 2 
      in 2 * y * z

{- On the use of infix $ :: (a -> b) -> a -> b i.e. f $ a = f a where f :: a -> b -}
dollarSign = (3^) $ 2 + 2 -- 3^(2 + 2)

dollarSign' = (3^) 2 + 2  -- (3^2) + 2

{- Sectioning -}

plus3 = (+3)

operation = plus3 4

--To refer to an operator on its own i.e. as a function
plus = (+)