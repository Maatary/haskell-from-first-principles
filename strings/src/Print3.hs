module Print3 where

  
  
{-
  String is a type alias for [char] 
-}

myGreetings :: String
myGreetings = "hello " ++ " world!"

hello :: String
hello = "Hello"

world :: String
world = "world"

main :: IO ()
main = do
  putStrLn helloWorld
  where
    helloWorld = concat [hello, " ", world]



{- Type Declaration in Local binding

  We can declare the type of the variable in the where and let clause

-}


topLevelFunctionWhere :: Integer -> Integer
topLevelFunctionWhere x = x + woot + topLevelValue
 where
      woot :: Integer -- declaring the type in the where clause
      woot = 10


topLevelFunctionLet :: Integer -> Integer
topLevelFunctionLet x =
  let
      woot :: Integer -- declaring the type in the Let clause
      woot = 10

  in x + woot + topLevelValue

topLevelValue :: Integer
topLevelValue = 5


