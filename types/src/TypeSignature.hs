{-# LANGUAGE NoMonomorphismRestriction #-}

module TypeSignature where

{-
For the following expressions, please add a type signature. 
You should be able to rely on GHCi type inference to check your work, 
although you might not have precisely the same answer as GHCi gives (due to polymorphism, etc).
-}

--(1)
functionH :: [a] -> a
functionH (x:_) = x
-- functionH :: [a] -> a


--(2)
functionC :: Ord a => a -> a -> Bool
functionC x y = 
  if (x > y) 
  then 
    True 
  else 
    False 
-- functionC :: (Ord a) => a -> a -> Bool


--(3)
functionS :: (a, b) -> b
functionS (_, y) = y
-- functionS :: (a, b) -> b

 