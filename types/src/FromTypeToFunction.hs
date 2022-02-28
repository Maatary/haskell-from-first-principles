module FromTypeToFunction where



{-
You will be shown a type and a function that needs to be written.
Use the information the type provides to determine what the function should do.
We’ll also tell you how many ways there are to write the function.

Syntactically different but semantically equivalent implementations are not counted as being different.
For example, writing a function one way and then rewriting the semantically identical function,
but using anonymous lambda syntax does not count as two implementations.
-}

--(1) There is only one function definition that type checks and doesn't go into an infinite loop when you run it
i :: a -> a
i x = x

--(2) There is only one version that works
c :: a -> b -> a
c a b = a

--(3) Given alpha equivalence, are the variables c'' and c (from the previous exercise) the same thing?
-- YES
c'' :: b -> a -> b
c'' b a  = b

--(4) Only one version works
c' :: a -> b -> b
c' a b = b

--(5) There are multiple possibilities, at least two of which you’ve seen in previous chapters
r :: [a] -> [a]
r l = l  -- r = reverse l

--(6) Only one version will type check
co :: (b -> c) -> (a -> b) -> a -> c
co bToc aTob a = bToc $ aTob a


--(7) One version will type check
a :: (a -> c) -> a -> a
a aToc a = a

--(8) One version will type check
a' :: (a -> b) -> a -> b
a' aTob a = aTob a

