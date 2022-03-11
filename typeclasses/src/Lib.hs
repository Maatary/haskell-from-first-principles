{-#LANGUAGE NoMonomorphismRestriction#-}
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


x :: Bool
x = True == False 

list :: [Int] 
list = [1]


test :: (Eq a, Num a) => a -> Bool
test = (==) 2

test2 :: Bool
test2 = (1,2) == (1,2)