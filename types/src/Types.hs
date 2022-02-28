-- {-# LANGUAGE FlexibleContexts #-}
module Types where



{- Exercise 1: Fill in the correct type signature ? -}

-- a) not :: Bool -> Bool
-- b) length :: [a] -> Integer
-- c) concat :: [[a]] -> [a] or :: Foldable t => t[a] -> [a]
-- d) head [a] -> a
-- e) (<) :: Ord a =>  a -> a ->  Bool

nonsense :: Bool -> Integer
nonsense True  = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer -- haskell automated currying i.e. the compiler convert it to the `anonNested` form below.
curriedFunction i b = i + nonsense b

uncurriedFunction :: (Integer, Bool) -> Integer  -- manually uncurry the previous function
uncurriedFunction (i, b) = i + nonsense b

anonymous :: Integer -> Bool -> Integer -- anonymous function leveraging automated haskell currying
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer -- manual uncurried. What the compiler does to `curriedFunction` above
anonNested = \i -> \b -> i + (nonsense b)

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b


{- Exercise 2: Answer the Type of Expression ? -}
-- 1)
f :: a -> a -> a -> a ; f = undefined
x :: Char; x = undefined
-- :t f x
-- :: Char -> Char -> Char

-- 2)
g :: a -> b -> c -> b; g = undefined
-- :t g 0 'c' "woot"
-- :: Char

-- 3)
h :: (Num a, Num b) => a -> b -> b ; h = undefined
-- :t h 1.0 2
-- :: Num b => b -- because literal values are polymorphic and the compiler always takes the most generic type i.e. 2 can be any numeric dataTypes

-- 4)
-- :t h 1 (5.5::Double) -- because double is ascribed and Num Double exist i.e. conform to the constraint.
-- :: Double

-- 5)
jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
-- :t jackal "hello" "your"
-- :: [Char]

-- 6)
-- :t jackal "keyword"
-- :: Eq b => b -> [Char]

-- 7)
kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
-- :t kessel 1 2
-- :: (Ord a, Num a) => a -- because we know it is a numeric from the polymorphic literal value, and it has an Ordinal Constraint not going away (num does not make one ordinal, and ordinal does not make one  Num)

-- 8)
-- :t :t kessel 1 (2::Integer)
-- :: (Ord a, Num a) => a  -- because only b is concrete fixed to Integer, so the same as above apply here. The result is a .

-- 9)
-- :t kessel (1::Integer) 2
-- :: Integer


{- Exercise 3: Parametric -}

-- (1) Can A Function :: a -> a have more than one implementation, let alone one that change the return type ?
-- No

-- (2) A function :: a -> a -> a  has two possible implementation only, what are they ?
p1 :: a -> a -> a
p1 x y = x

p1' :: a -> a -> a
p1' x y = y

-- (3) a Function :: a -> b -> b. How many implementations can it have? Does its behavior change when the types of a and b change?
-- 1 implementation possible. No the behavior would not change.
p2 :: a -> b -> b
p2 x y = y
  

{- Exercise 4: Infer the type of the function (Answer in the type signature) -}

-- Because " yo" is a [Char]
myConcat :: [Char] -> [Char]
myConcat x = x ++ " yo"

-- Because 3 and 5 are polymorphic constant and (/) is a function  of the Fractional type class 
myMult :: Fractional a => a -> a 
myMult x = (x / 3) * 5

-- Because take takes an Int
myTake :: Int -> [Char] 
myTake x = take x "hey you"

-- Because length returns Int
myCom :: Int -> Bool 
myCom x = x > (length [1..10])

-- Because 'z' is the Char
myAlph :: Char -> Bool 
myAlph x = x < 'z'


