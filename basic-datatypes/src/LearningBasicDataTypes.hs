module LearningBasicDataTypes where


-- Basic Data Types definition
data Mood = Blah | Woot deriving (Show)


{- Glimpse at Pattern Matching & working with algebraic datatypes

  Given the type Mood define a above

  1. We want to write a function that change the Mood given a specific Mood (i.e. act like a not),
     State what's wrong  with the following type signature ? and fix it ?
     changeMood :: Mood -> Woot  -- Type signature contains type not values Woot is a value (see correction below)

  2. Using pattern matchin we wrote the following function body
     State what's wrong with it and fix it
     changeMood Mood = Woot -- Pattern matching is on value not type, Mood is a type (see correction below)
     changeMood _    = Blah

-}

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah


-- Example of Type alias
type Name = String
data Pet  = Cat | Dog Name deriving (Show)

-- Pattern Matching of Pet
makeSound :: Pet ->  String
makeSound Cat      = "Miwow"
makeSound (Dog _)  = "Woof Woof"



e = fromIntegral 2 + 2.0


-- Playing around - inspired from https://wiki.haskell.org/Constructor
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

tree :: Tree Integer
tree = Node 2 (Node 3 Leaf Leaf) Leaf


{- Note: converting most thing (including Numeric) to String is done with show. -}


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

awesome :: [[Char]]
awesome = ["Papuchon", "curry", ":)"]

also :: [[Char]]
also = ["Quake", "The Simons"]

allAwesome :: [[[Char]]]
allAwesome = [awesome, also]



-- 1) length :: Foldable t => t a -> Int

-- 3) (a) 6 / length [1,2,3] can't work because / takes two fractional and length returns an Int.

-- 4) (b) 6 div length [1,2,3] would fix that



-- 8)
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x


-- 9)
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

-- 10)
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y =
  let
    a = fst x
    b = snd x
    c = fst y
    d = snd y
  in
    ((b, d), (a, c))

-- 10) This is already pattern matching i.e. (a, b) (c, d) are the Data Constructor here.
f'' :: (a, b) -> (c, d) -> ((b, d), (a, c))
f'' (a, b) (c, d) = ((b, d), (a, c))

{- Correcting syntax -}

-- 1)

x' = (+)

f' xs =
  (x') w 1
  where w = length xs

-- 2)

id' = \x -> x

-- 3) this uses pattern matching i.e. (a, b) is a Data Constructor.

fst' (a,b) = a



class Pretty a where
  pretty :: String -> a
  
instance Pretty Integer where
  pretty x =  read x
  
instance Pretty Double where
  pretty x =  read x 
  
instance Pretty Mood where
  pretty x =  Blah
  
--trial1 = pretty "2.0"

--trial0 = read ("2"::String)
--trial2 = fromInteger(2::Integer)


--trial = pretty "2" 