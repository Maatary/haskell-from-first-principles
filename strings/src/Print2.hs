module Print2 where

{-
  The do syntax is a special syntax that allows for sequencing actions.
  It is most commonly used to sequence the actions that constitute your program, some of which will necessarily perform effects such as printing to the screen (that’s why the obligatory type of main is IO ()).
  do notation isn’t strictly necessary, but it often makes for more readable code than the alternatives.

-}
main :: IO () 
main = do 
  putStrLn "Count to four for me:" 
  putStr "one, two" 
  putStr ", three, and" 
  putStrLn " four!"



