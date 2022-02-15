# Fundamentals


## What Are Types For ?


## Type Signature of Numeric Values


 - When we query the types of numeric values, we see type class information instead of a concrete type, because the compiler doesn’t know which specific numeric type a value is until the type is either declared or the compiler is forced to infer a specific type based on the function (i.e. context). **Indeed, as mentioned in the previous chapter notes, numeric values literals are polymorphic.**
 

 - For example, `13` may look like an integer to us, but that would only allow us to use it in computations that take integers (and not, say, in fractional division). For that reason, **the compiler gives it the type with the broadest applicability** (_**most polymorphic**_) and **says it’s a constrained polymorphic value with the** `type Num a => a`:
 
   > `λ> :t 13`  
   >`13 :: Num p => p`  
   


 - See previous chapter [Numeric Datatypes Section](/basic-datatypes/fundamentals.md#numeric-datatypes), for more detail on numeric values 


## The Function Type


 - **The arrow**, `->`, **is the type constructor for functions** in Haskell. It’s _baked_ into the language, but syntactically it works in very much the same way as all the other types. 


 - It’s a **type constructor**, like `Bool`, except the `->` **type constructor** _takes arguments_ and has _no data constructors_
 
   > `λ> :info (->)`  
   > `data (->) a b`


 - We saw earlier that the _tuple constructor_ needs to be **applied** to two values in order to construct a tuple. **A function** must similarly have two arguments—**one input and one result**—in order to be a function. 


 - Unlike the _tuple constructor_, though, **the function type has no data constructors**. The value that shows up at term level is the function. **Functions are values**.


 - **The hallmark of a function is that it can be applied**, and the structure of the type demonstrates this. The **arrow** is an **infix operator** that has **two parameters** and _associates to the right (although function application is left associative)_. 

 - **The parameterization** suggests that we will apply the function to some argument **whose type** that will be bound to the first type parameter, `a`, with the second **type parameter**, `b`, representing the return or _result type_.



## Type class constraint

 - Looking at the **type signature** of the infix operators `(+)` and `(/)`

   > `λ> :type (+)`    
   > `(+) :: Num a => a -> a -> a` 
   > 
   > `λ> :type (/)`    
   > `(/) :: Fractional a => a -> a -> a`   


 - The above example can be described casually as, _addition_ takes one **numeric argument**, adds it to a second **numeric argument of the same type**, and returns **a numeric value of the same type as a result**. Similarly, the _fractional division_ function takes **a fractional value**, divides it by a **second fractional value**, and returns a third **fractional value as a result**. This isn’t precise, however. 


 - **Note that the compiler gives the least specific and most general type it can**. Instead of limiting this function to a concrete type, we get **a type class-constrained polymorphic type variable**.


 - **Type Classes** offers a standard set of functions that can be used across several concrete types. Hence, **when a type class is constraining a type variable in this way**, **the variable could represent any of the concrete types that have instances of that type class**, so that specific operations on which the function depends are defined for that type.


 - We say it’s constrained, because we still don’t know the concrete type of a, but we do know it can only be one of the types that has the required type class instance. 


 - For instance, in `x = 1 :: Num p => p`, we say `x` has **a** type `p` for which there must be an implementation of the Num Type Class. That is, `p` is constrained.


 - This generalization (trough type class) of numberhood is what lets us use the same _numeric literals_ to represent numbers of different types. 


 - We can start with a polymorphic Num a => a value and then create specific kinds of numbers with a concrete type using the :: to assign a type to the value:

   > `λ> fifteen = 15`    
   > `λ> :t fifteen`    
   > `fifteen :: Num a => a`   
   > 
   > `λ> fifteenInt = fifteen :: Int`   
   > `λ> fifteenDouble = fifteen :: Double`
   > 
   > `λ> :t fifteenInt`    
   > `fifteenInt :: Int`
   > 
   > `λ> :t fifteenDouble`    
   > `fifteenDouble :: Double`   

 
 - We went from `Num a => a` to **Int** and **Double**. This works, because Int and Double each have an instance of the **Num Type Class**.


 - A type signature might have multiple type class constraints on one or more of the variables.
 

 - In the first example above, there are two constraints, one for each variable. **Both a and b must have instances of the Num type class**. In the second example, both of the constraints are on the one variable a—that is, **a must be a type that implements both Ord and Num**.



## Functions Currying

 - **As in the lambda calculus**, arguments (plural) are a shorthand for the truth in Haskell: **all functions in Haskell take one argument and return one result**.


 - **Other programming languages,** typically allow you to define functions that can take multiple arguments. There is no support for this built into Haskell. **Instead, there are syntactic conveniences that construct curried functions by default.**


 - Currying refers to the nesting of multiple functions, each accepting one argument and returning one result, which creates the illusion of multiple-parameter functions.


 - **The key is that in** `data (->) a b`, **like in lambda calculus**,  `b` **can be another function**. _Note that `a` can be another function as well but this is beside the point here._ That is, Taking the example of addition type signature as below:
 
   > `λ> :t (+)`  
   > `(+) :: Num a => a -> a -> a`  


 - **The way the -> type constructor for functions works means** `a -> a -> a` represents successive function applications, each taking one argument and returning one result. The difference is that the function at the outermost layer is returning another function that accepts the next argument. This is called currying.


 - **More specifically, The way the type constructor for functions, ->, is defined makes currying the default in Haskell**. This is because **it is an infix operator and right associative**. Since it associates to the right, types are implicitly parenthesized like so:
 
   > `f :: a -> a -> a`  
   >   
   > (associate to) 
   >   
   > `f :: a -> (a -> a)` 

    or 

   > `map :: (a -> b) -> [a] -> [b]`
   >
   > (associate to)
   >
   > `map :: (a -> b) -> ([a] -> [b])`
   
 


 - **The association here, or grouping into parentheses, is not to control precedence or order of evaluation;** it only serves to group the parameters into arguments and results, since there can only be one argument and one result per arrow. Since all the arrows have the same precedence, the associativity does not change the precedence or order of evaluation.



 - **Explicit parenthesization**, **as when an input parameter is itself a function** (such as in map, above), may be used to indicate order of evaluation, but the implicit associativity of the function type does not mean the inner or final set of parentheses, i.e., the result type, evaluates first. Application is evaluation; in other words, the only way to evaluate anything is by applying functions, and **function application is left associative**.


## Partial Application

 - Thanks to curring we can partially apply function. 


 - The ability to apply only some of a function’s arguments is called partial application. This lets us create a new function from a function by applying some of the arguments.


## Manual Currying and UnCurrying

 - **Functions in Haskell are curried by default**, but it is possible to **"uncurry"** them. 


 - UnCurrying means, for example, un-nesting two functions and replacing them with a tuple of two values (these would be the two values you want to use as arguments). 


 - If we uncurry the `(+)` function, the type changes from `Num a => a -> a -> a` to the type `Num a => (a, a) -> a`.


 - **Uncurried functions: One function, many arguments.** 


 - **Curried functions: Many functions, one argument apiece.**


 - Illustration _(All the function aside from nonsense are equivalent)_: 
 
   ```haskell
   nonsense :: Bool -> Integer
   nonsense True  = 805 
   nonsense False = 31337
   
   curriedFunction :: Integer -> Bool -> Integer -- haskell automated currying i.e. the compiler convert it to the `anonNested` form below. 
   curriedFunction i b = i + nonsense b 
   
   uncurriedFunction :: (Integer, Bool) -> Integer -- manually uncurry the previous function
   uncurriedFunction (i, b) = i + nonsense b 
   
   anonymous :: Integer -> Bool -> Integer  -- anonymous function leveraging automated haskell currying
   anonymous = \i b -> i + (nonsense b) 
   
   anonNested :: Integer -> Bool -> Integer -- manual uncurried. What the compiler does to `curriedFunction` above
   anonNested = \i -> \b -> i + (nonsense b)
   ```

## Automated Currying & Uncurrying

 - Prelude has functions to automatically currying and uncurrying function, namely `curry` and `uncurry`. We write our own version to illustrate the all point.

   ```haskell
   curry' :: ((a, b) -> c) -> a -> b -> c
   curry' f a b = f (a, b)
   
   uncurry' :: (a -> b -> c) -> (a, b) -> c
   uncurry' f (a, b) = f a b
   ```

   > `λ> :t fst`  
   > `fst :: (a, b) -> a` 
   > 
   > `λ> :t curry' fst`   
   > `curry' fst :: c -> b -> c`
   > 
   > `λ>  curry' fst 1 2`   
   > `1`
 
   > `λ> :t (+)`  
   > `(+) :: Num a => a -> a -> a`  
   >  
   > `λ> :t uncurry' (+)`  
   > `uncurry' (+) :: Num c => (c, c) -> c` 
   > 
   > `λ> :t uncurry' (+) (1,2)`  
   > `uncurry' (+) (1,2) :: Num c => c`   


## Sectioning

 - **The term sectioning specifically refers to the partial application of infix operators**, which has **a special syntax** that allows you to **choose whether you’re partially applying the operator to the first or the second argument**.

    ```haskell
    λ> x = 5
    λ> y = (2^)
    λ> z = (^2)
    λ> y x
    32
    λ> z x
    25
    
    λ> :t (^)
    (^) :: (Integral b, Num a) => a -> b -> a
    
    λ> :t (^2)
    (^2) :: Num a => a -> a
    
    λ> :t (2^)
    (2^) :: (Integral b, Num a) => b -> a
   
   
    λ> celebrate = "woot!"
    λ> celebrate = (++ "woot!")
    λ> celebrate "naptime "
    "naptime woot!"
    ``` 

 - The **infix nature of the operator** is what allows to choose which arguments to apply first. Otherwise, the left to right rule of function application applies, that is, function application is left associative.


 - **The sectioning syntax** exists to allow you some freedom as to which argument of a binary operator you apply the function to.

 - Normal function in prefix form, or infix operator used in their prefix form, can't use sectioning, it would be classic partial application that respect the left to right associativity i.e. first argument get applied first.

