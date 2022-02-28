{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType1 where

{-
  All function applications return a value.
  Determine the values returned by these function applications and the types of those values.
-}

a :: Num a => a
a = (* 9) 6
-- :: Num a => a


b :: Num a => (a, [Char])
b = head [(0,"doge"),(1,"kitteh")]
-- :: Num a => (a, String)

c :: (Integer, [Char])
c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
-- :: (Integer, String)

d :: Bool
d = if False then True else False
-- :: Bool

e :: Int
e = length [1, 2, 3, 4, 5]
-- :: Int

f :: Bool
f = length [1, 2, 3, 4] > length "TACOCAT"
-- :: Bool
