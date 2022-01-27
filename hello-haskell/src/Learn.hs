module Learn( ) where

sayHello ::  String -> IO ()
sayHello x = 
  putStrLn ("Hello, " ++ x ++ "!")

{- First Exploration -}
half x = x / 2

square x =  x * x

triple x = x * 3

myPi x = 3.14 * x^2

aPi x = pi * x ^ 2

{- Infix function i.e. Operator and PreFix Function in Infix Position -}
functionInInfix = 10 `div` 4

normalFunction  = div 10 4

{- Precedence -}

{- 

  perimeter' = perimeter
  
  Because * is precedence 8 and + 7

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