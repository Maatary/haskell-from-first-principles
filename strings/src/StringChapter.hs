module StringChapter where


{- Correct Syntax

   Read the syntax of the following functions, and decide whether it will compile.
   Test them in your REPL, and try to fix the syntax errors where they occur:

   1. ++ [1, 2, 3] [4, 5, 6]      -- wrong
   2. '<3' ++ ' Haskell'          -- wrong
   3. concat ["<3", " Haskell"]   -- good

   Answer
   1. (++) [1, 2, 3] [4, 5, 6]
   2. "<3" ++ " Haskell"
   3. concat ["<3", " Haskell"]

-}

{- Operation on List from the stdLib

   λ> :t (:) --cons
   (:) :: a -> [a] -> [a]

   λ> :t (!!) --valueAtIndex
   (!!) :: [a] -> Int -> a

   λ> :t head -- first element
   head :: [a] -> a

   λ> :t tail
   tail :: [a] -> [a]

   λ> :t drop
   drop :: Int -> [a] -> [a]

   λ> :t take
   take :: Int -> [a] -> [a]
-}

aCons :: [Char]
aCons = 'c' : "chris" -- "Chris"

valAtIndex :: Char
valAtIndex = "hello" !! 4 -- 'o'

aHead :: Char
aHead = head "hello" -- 'h'

-- everything but first element
aTail :: [Char]
aTail = tail "hello" -- ello

-- Drop the specified number of element from the start
aDrop :: [Char]
aDrop = drop 3 "hello" -- "lo"

-- Take the specified number of element from the start
aTake = take 4 "hello" -- hell


{-
  Readying Syntax
-}

{- Fix the expression if needed.

  1. concat [[1, 2, 3], [4, 5, 6]] -- ok
  2. (++) [1, 2, 3] [4, 5, 6]      -- ok
  3. (++) "hello" " world"         -- ok
  4. ["hello" ++ " world"]         -- ok
  5. (!! 4) "hello"                -- ok -- here we do a `right sectioning` of the operator '!!'
  6. (!!) "hello" 4                -- ok -- prefix an infix operator
  7. take 4 "lovely"               -- ok
  8. take 3 "awesome"              -- ok

-}


{- Which result corresponds to which expression evaluation

  concat [[1 * 6], [2 * 6], [3 * 6]] ==  [6,12,18]
  "rain" ++ drop 2 "elbow" ==  "rainbow"
  10 * head [1, 2, 3] = 10
  (take 3 "Julie") ++ (tail "yes") == "Jules"
  concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]] ==  [2,3,5,6,8,9]

-}


{- Write the expression that evaluate when applied to the given inputs, returns the given results

  tail "hello world" == drop 1 "hello world" == "ello world"
  "Curry is awesome" ++ "!" == "Curry is awesome!"
  "Curry is awesome!" !! 4 == "y"
  drop 9 "Curry is awesome!" == "awesome!"

-}

{- Write the function that generalize the behavior below as function taking a string as parameter.

  a) Given "Curry is awesome"
     Return "Curry is awesome!"

  b) Given "Curry is awesome!"
     Return "y"

  c) Given "Curry is awesome!"
     Return "awesome!"


-}

addExclamation :: [Char] -> [Char]
addExclamation sentence = sentence ++ "!"

fifthLetter :: [a] -> a
fifthLetter sentence = sentence !! 4

drop9 :: [a] -> [a]
drop9 = drop 9


{- Write a function of type String -> Char that returns the third character in a String. -}
thirdLetter :: String -> Char
thirdLetter sentence = sentence !! 2

{- Change the above function so that the string operated on is always the same and the variable represents the number of the letter you want to return -}
letterIndexInAwesome :: Int -> Char
letterIndexInAwesome index = "Awesome" !! index


{- Reverse "Curry is awesome"

  I'm using f . g just for the sake of it. -- f . g = f(g(x)) or in Haskell f g x = f $ g x

-}

takeDrop :: Int -> Int -> [a] -> [a]
takeDrop d t = take t . drop d

dropTake :: Int -> Int -> [a] -> [a]
dropTake t d = drop d . take t

{- Reverse "Curry is Awesome"

  Using the take and drop functions we looked at above, see if you can write a function called rvrs.

    - an abbreviation of “reverse,” used because there is already a function called reverse in Prelude,
      so if you give your function the same name, you’ll get an error message)

  rvrs should take the string "Curry is awesome" and return the result "awesome is Curry".
  This doesn’t need to, and shouldn’t, work for reversing the words of any sentence.
  You’re expected only to slice and dice this particular string with take and drop.

  (Page 85).


-}
rvrs :: [Char]
rvrs  =
  let
    curryIsAwesome = "Curry is awesome"
    curry = take 5 curryIsAwesome
    is = dropTake 8 6 curryIsAwesome
    awesome = drop 9 curryIsAwesome
  in concat [awesome, " ",  is, " ", curry ]


{- Put rvrs in a main -}

main :: IO ()
main = putStrLn rvrs