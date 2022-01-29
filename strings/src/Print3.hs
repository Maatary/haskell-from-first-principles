module Print3 where

  
  
{- 
  Note:
  -----
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
