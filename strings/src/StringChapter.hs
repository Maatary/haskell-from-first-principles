module StringChapter where


{-
  Readying Syntax
-}

-- 1) Fix the expression if needed.

-- concat [[1, 2, 3], [4, 5, 6]]
-- (++) [1, 2, 3] [4, 5, 6]
-- (++) "hello" " world"
-- ["hello" ++ " world"]
-- (!! 4) "hello"
-- (!!) "hello" 4
-- take 4 "lovely"
-- take 3 "awesome"

-- 2) Which result corresponds to which expression evaluation

-- concat [[1 * 6], [2 * 6], [3 * 6]] ==  [6,12,18]
-- "rain" ++ drop 2 "elbow" ==  "rainbow" 
-- 10 * head [1, 2, 3] = 10
-- (take 3 "Julie") ++ (tail "yes") == "Jules"
-- concat [tail [1, 2, 3], tail [4, 5, 6], tail [7, 8, 9]] ==  [2,3,5,6,8,9]

-- 3) Write the expression that evaluate when applied to the given inputs, returns the given results

-- tail "hello world" == drop 1 "hello world" == "ello world"

-- "Curry is awesome" ++ "!" == "Curry is awesome!"

-- "Curry is awesome!" !! 4 == "y" 

-- drop 9 "Curry is awesome!" == "awesome!"


-- 4) Rewrite the previous expression as function taking a string as parameter

addExclamation :: [Char] -> [Char]
addExclamation sentence = sentence ++ "!"

fifthLetter :: [a] -> a
fifthLetter sentence = sentence !! 4

drop9 :: [a] -> [a]
drop9 = drop 9

thirdLetter :: [a] -> a
thirdLetter sentence = sentence !! 2

letterIndexInAwesome :: Int -> Char
letterIndexInAwesome index = "Awesome" !! index


-- 5) Reverse "Curry is awesome"

dropTake :: Int -> Int -> [a] -> [a]
dropTake d t = take t . drop d

takeDrop :: Int -> Int -> [a] -> [a]
takeDrop t d = drop d . take t

-- The function expect as input "Curry is awesome"
rvrs  =
  let
    curryIsAwesome = "Curry is awesome"
    currey = take 5 curryIsAwesome
    is = takeDrop 8 6 curryIsAwesome
    awesome = drop 9 curryIsAwesome
  in concat [awesome, " ",  is, " ", currey ]


-- 6) Put rvrs in a main

main :: IO ()
main = putStrLn rvrs