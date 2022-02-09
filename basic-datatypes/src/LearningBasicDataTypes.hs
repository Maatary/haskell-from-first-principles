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

