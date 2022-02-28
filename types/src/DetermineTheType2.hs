{-# LANGUAGE NoMonomorphismRestriction #-}


module DetermineTheType2 where

-- (1) What's the type of w

x :: Num p => p
x = 5

y :: Num a => a
y = x + 5

w :: Num a => a
w = y * 10
-- :: Num a => a


-- (2) What's the type of z1
x1 :: Num p => p
x1 = 5

y1 :: Num a => a
y1 = x1 + 5

z1 :: Num a => a -> a
z1 y1 = y1 * 10
-- :: Num a => a -> a

-- (2) What's the type of f2
x2 :: Num p => p
x2 = 5

y2 :: Num a => a
y2 = x2 + 5

f2 :: Fractional a => a
f2 = 4 / y2
-- :: Fractional a => a


 -- (3) what's the type of f3
x3 :: [Char]
x3 = "Julie"

y3 :: [Char]
y3 = " <3 "

z3 :: [Char]
z3 = "Haskell"

f3 :: [Char]
f3 = x3 ++ y3 ++ z3
-- :: [Char]



