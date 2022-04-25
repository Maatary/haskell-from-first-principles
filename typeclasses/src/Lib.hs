{-# LANGUAGE NoMonomorphismRestriction#-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTSyntax #-}

module Lib where


{-
list :: [a]
list = [undefined]
-}


-- GADTSyntax i.e. data Me a = Me a
data Me a where
  Me :: forall e. e -> Me e
  deriving Show


-- This works because Num a => has all the numeric literals has inhabitant.
me :: forall a.  Num a => Me a
me = Me 2

-- Does not work, because we know nothing about a. Compared to the above, Eq a => give you no about a.
--mea ::  Eq a  => Me a
--mea = Me '2' 
  
instance forall a. Eq (Me a) where 
  (==) (Me x) (Me y) = True  

meaEq =  Me "1" == Me "2"

-- This would explain why the above work
data Record a where -- data Record a = Record { idx :: a -> a }
  Record :: forall b. {idx :: b -> b } -> Record b

record :: forall a. Record a
record = Record { idx = \x -> x }

use :: a -> a
use  = idx record -- field selection in GADT syntax


listFunA :: forall a. [a -> a]
listFunA = [\x -> x]

aFun :: forall a. a -> a
aFun = id
-- Example of GADTSyntax

data Maybe' a where
    Nothing :: Maybe' a
    Just :: a -> Maybe' a
    
data Tuple a b where
    Tuple :: a -> b -> Tuple a b
    deriving Show

-- Example of Record Syntax as Record of Functions   
--data Same a = Same {
--  (===) :: a -> a -> Bool
--}

-- Example of GADTSyntax + Record syntax
--data Person' where
--  Person' :: { name' :: String, age' :: Int } -> Person'
--  deriving Show

-- GADTSyntax + Record Syntax  
data Same b where
  Same :: forall a. { (===) :: a -> a -> Bool } -> Same a


data  DatumType a b = Datum a b deriving Show

instance forall a b . (Eq a, Eq b) => Eq (DatumType a b) where
  (==) (Datum x y) (Datum z e) =  x == z && y == e



sameDatumType :: (Eq a, Eq b) => Same (DatumType a b)
sameDatumType = Same {
  (===) = \(Datum x y) (Datum z e) -> x == z && y == e
}

sameDatumTypeab :: Same a -> Same b -> Same (DatumType a b)
sameDatumTypeab (Same eqa) (Same eqb) -- pattern match a Same a gives you access to its function i.e. eqa
   = Same $ \(Datum x y) (Datum z e) -> eqa x z && eqb y e

{-size :: forall a. [a] -> Int
size l = length l


myId :: a -> b -> a
myId x y = x-}

--xa :: forall a. [a]
--xa = [1]

-- :set -XExplicitForAll
-- :set -fprint-explicit-foralls
-- :set -XTypeApplications --Only works when the type signature is defined explicitly.

-- This is RankNTypes Stuff and is useless
{-
f :: (forall a. [a]) -> (forall a. [a])
f l = l
-}

-- For accessing GHC Core translation
{-
alias ghci-core="ghci -ddump-simpl -dsuppress-idinfo \
-dsuppress-coercions -dsuppress-type-applications \
-dsuppress-uniques -dsuppress-module-prefixes"
-}