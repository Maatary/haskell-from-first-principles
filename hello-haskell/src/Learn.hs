module Learn where

sayHello ::  String -> IO ()
sayHello x = 
  putStrLn ("Hello, " ++ x ++ "!")
    
triple x = x * 3

myPi x = 3.14 * x^2

aPi x = pi * x ^ 2