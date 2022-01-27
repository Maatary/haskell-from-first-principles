module LetAndWhere where


printInc2 :: (Show a, Num a) => a -> IO ()
printInc2 n =
  let
    plusTwo = n + 2
  in
    print plusTwo

printInc2' n =
  print plusTwo
  where 
        plusTwo = n + 2
