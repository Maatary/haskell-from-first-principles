# Fundamentals

## What are Type Classes ?

 - **Type classes** and **types** in Haskell **are, in a sense, opposites**. **Whereas a declaration of a type defines how that type in particular is created**, **a declaration of a type class defines how a set of types are consumed or used in computations**.


 - **This tension is related to the _expression problem_**, which is about defining code in terms of **_how data is created_** or **_processed_**.


 - **As Philip Wadler put it,** "_The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety (e.g., no casts)_".


 - **It may help to think of type classes as being like interfaces to data that can work across multiple datatypes**. The latter facility is why type classes are a means of **ad hoc polymorphism** — **ad hoc because type class code is dispatched by type**.


 - **Type classes** allow us to **generalize over a set of types** in order to **define** and **execute** **a standard set of features for those types**.
 

 - **For example,** the ability to test values for equality is useful, and we’d want to have it for data of various types. In fact, we can test any data of a type that implements the type class known as Eq for equality. We do not need separate equality functions for each different type of data; as long as our datatype implements, or instantiates, the Eq type class, we can use the standard functions == and /=. 


 - **Similarly,** all the numeric literals and their various types implement a type class called Num, which defines a standard set of operators that can be used with any type of numbers.


 - **Briefly stated,** what it means for a type to have an “instance” of a type class, is that there is code that defines how the values and functions from that type class work for that type. 


 - **When you use a type class method with one of the types that has such an instance, the compiler looks up the code that dictates how the function works for that type**.


## Checking what Types Classes does a Type Implements


 - Simply type `:info` followed by the type

    ```haskell
    λ> :info Bool  
    type Bool :: *     
    data Bool = False | True     
            -- Defined in ‘ghc-prim-0.6.1:GHC.Types’     
    instance Eq Bool -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’  
    instance Ord Bool -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’  
    instance Enum Bool -- Defined in ‘GHC.Enum’   
    instance Show Bool -- Defined in ‘GHC.Show’   
    instance Read Bool -- Defined in ‘GHC.Read’   
    instance Bounded Bool -- Defined in ‘GHC.Enum’   
    ```
 
 - The information includes the data declaration for Bool and which type classes it already has instances of. It also tells where the datatype and its instances are defined for the compiler.


 - **Each instance represents a type class that Bool implements**


 - **The instances are the unique specifications** of how Bool makes use of the methods from that type class.


## Eq Type Class

 - In Haskell, equality is implemented with a type class called Eq. 

 - Some programming languages bake equality into every object in the language, but some datatypes do not have a sensible notion of equality, so Haskell does not encode equality into every type. 
 

 - Eq is defined this way:

    ```haskell
    λ> :i Eq
    type Eq :: * -> Constraint
    class Eq a where
      (==) :: a -> a -> Bool
      (/=) :: a -> a -> Bool
      {-# MINIMAL (==) | (/=) #-}
            -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’
    instance Eq a => Eq [a] -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’
    instance Eq Word -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’
    instance Eq Ordering -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’
    instance Eq Int -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’
    instance Eq Float -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’
    instance Eq Double -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’
    instance Eq Char -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’
    instance Eq Bool -- Defined in ‘ghc-prim-0.6.1:GHC.Classes’
    ```

 - GHCi tells us we have a type class called **Eq** that specifies **two basic functions, equality and non-equality**, **and gives their type signatures**. Next, it prints out all the instances of Eq that it knows about.


 - **Any type that has an instance of this type class must implement the methods of Eq.**

 - **The types of** `==` and `/=` in `Eq` tell us something important about these functions: 

   > `(==) :: Eq a => a -> a -> Bool`  
   > `(/=) :: Eq a => a -> a -> Bool`  


 - Given these types, we know that they can be used for any type `a` that implements the `Eq` type class. 


 - We also know that both functions **will take two arguments of the same type `a` and return a value of type Bool**. We know they have to be the same because `a` must equal `a` in the same type signature.


 










