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
-- :: Num b => b -- because literal values are polymorphic and the compiler always takes the most generic type.

-- 4)
-- :t h 1 (5.5::Double) -- because double is ascribed and Num Double exist i.e. conform to the constraint.
-- :: Double


-- 5)
jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
-- :t jackal "hello" "your"
-- :: [Char]
