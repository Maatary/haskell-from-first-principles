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

curriedFunction :: Integer -> Bool -> Integer 
curriedFunction i b = i + nonsense b 

uncurriedFunction :: (Integer, Bool) -> Integer 
uncurriedFunction (i, b) = i + nonsense b 

anonymous :: Integer -> Bool -> Integer 
anonymous = \i b -> i + (nonsense b) 

anonNested :: Integer -> Bool -> Integer 
anonNested = \i -> \b -> i + (nonsense b)

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b