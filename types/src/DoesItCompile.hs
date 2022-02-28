{-# LANGUAGE NoMonomorphismRestriction #-}

module DoesItCompile where

{- Does it compile ? if not fix it if possible -}

--a) No . Function application issue and redundant use of $.
--bigNum = (^) 5 $ 10
--wahoo = bigNum $ 10


bigNum :: (Integral b, Num a) => b -> a
bigNum = (^) 5 -- removed 10 to make bigNum a function. Reminder (^) :: (Integral b, Num a) => a -> b -> a.

wahoo :: Num a => a
wahoo = bigNum 10 -- at this point bigNum is a function.

--b) Yes
--x4 = print
--y4 = print "woohoo!"
--z4 = x4 "hello world"

x4 :: Show a => a -> IO ()
x4 = print

y4 :: IO ()
y4 = print "woohoo!"

z4 :: IO ()
z4 = x4 "hello world"

--c) No. Function application issue.
--a = (+)
--b = 5
--c = b 10
--d = c 200

a :: Num a => a -> a -> a
a = (+)

b :: Num p => p
b = 5

c :: Num a => a -> a
c = a 10 -- b is not a function but a is, so we do a partial application here.

d :: Num a => a
d = c 200 -- c is at this point a partially applied function, so we apply it.
