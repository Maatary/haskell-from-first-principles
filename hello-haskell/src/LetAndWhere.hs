
-- ignoring type defaulting pick by GHC warning at this point
{-# OPTIONS_GHC -Wno-type-defaults #-}

module LetAndWhere where

{-
  At the heart of it, Let and Where, are how local binding i.e. local variable are introduced in haskell code
  'let' introduces an expression, so it can be used wherever you can have an expression
  'where' is a declaration and is bound to a surrounding syntactic construct.
  The implication of the semantic difference between the two construct goes beyond this chapter.
  However a reference is provided below, should the reader feel the need to dive in right now. 
  https://wiki.haskell.org/Let_vs._Where
-}

printInc2 :: (Show a, Num a) => a -> IO ()
printInc2 n =
  let
    plusTwo = n + 2
  in
    print plusTwo

printInc2' :: (Show a, Num a) => a -> IO ()
printInc2' n =
  print plusTwo
  where 
        plusTwo = n + 2

letTest :: Integer
letTest = let x = 5 ; y = 2 in x + y

whereTest :: Integer
whereTest     = x * y
        where x = 1
              y = 2

letTest2 :: Double
letTest2 = 
  let x = 7
      y = negate x
      z = y * 10 in z / x + y

whereTest2 :: Double
whereTest2 = 
  z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn :: Integer
waxOn = 
  x * 5 
  where
     x = y ^ 2
     y = z + 8
     z = 7

aDiv :: Int
aDiv = a `div` length xs 
  where 
    a = 10 
    xs = [1,2,3,4,5]

