module LearningBasicDataTypes where


-- Basic Data Types definition
data Mood = Blah | Woot deriving (Show)

-- Glimpse at Pattern Matching
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah


-- Intro
type Name = String

data Pet  = Cat | Dog Name deriving (Show)

-- Playing around
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

tree :: Tree Integer
tree = Node 2 (Node 3 Leaf Leaf) Leaf


{- Note converting most thing (including Numeric) to String is done with show. -}


{- If Expressions -}

-- If Expression with Let clause.
greetIfCool00 :: String -> IO ()
greetIfCool00 coolness =
  if cool
    then putStrLn "eyyyyy. What's shakin'?"
    else putStrLn "pshhhh."
  where
    cool = coolness == "downright frosty yo"

-- If Expression with Where Clause.
greetIfCool01 :: String -> IO ()
greetIfCool01 coolness =
  let cool = coolness == "downright frosty yo"
  in
    if cool
    then putStrLn "eyyyyy. What's shakin'?"
    else putStrLn "pshhhh."

-- IF Expression including a local function
greetIfCool03 :: String -> IO ()
greetIfCool03 coolness = 
  if cool coolness 
    then 
      putStrLn "eyyyyy. What's shakin'?" 
    else 
      putStrLn "pshhhh." 
  where 
    cool v = v == "downright frosty yo"


{- chapter Exercises -}

-- 1) length :: Foldable t => t a -> Int

-- 3) (a) 6 / length [1,2,3] can't work because / takes two fractional and length returns an Int.

-- 4) (b) 6 div length [1,2,3] would fix that

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x


{-
  Note that to call this function with a negative number you must put parentheses e.g. myAbs (-4)
  or write  `myAbs $ negate 4` or `myAbs $ -4`
  This is because of the special treatment of minus.
  See https://stackoverflow.com/questions/28858161/why-doesnt-minus-work-for-operator-sections
-}
myAbs :: (Ord a, Num a) => a -> a
myAbs x =
  if x > 0
    then
      x
    else
      -x