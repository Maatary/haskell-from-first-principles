module Print1 where

{-
   main is not a function, it actually represent a series of instructions to execute, which can include applying functions and producing side effects. 
   We could say at this point that main is a procedure or a big instruction. 
-}

main :: IO ()
main =
   putStrLn "Hello World"