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

 - λ has functions to automatically currying and uncurrying function, namely `curry` and `uncurry`. We write our own version to illustrate the all point.

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

## Polymorphism


 - **Polymorph** is a word of relatively recent provenance. It was invented in the early 19th century from the Greek words **poly** for _"many"_ and **morph** for _"form."_ The **-ic suffix** in polymorphic means _made of._ So, **"polymorphic"** means **made of many forms.** In programming, this is understood to be in contrast to **monomorphic**, **made of one form.**


 - **Polymorphic type variables** give us the ability to implement expressions that can accept arguments and return results of different types without having to write variations on the same expression for each type.


 - **Broadly speaking, type signatures may have three kinds of types:** **concrete**, **constrained polymorphic**, or **parametrically polymorphic**. 


 - **In Haskell, polymorphism divides into two categories:** **parametric polymorphism** and **constrained polymorphism**. _The latter is also known as Ad-hoc polymorphism_


 - **Parametric polymorphism** is broader than ad-hoc polymorphism. **Parametric polymorphism** refers to type variables, or parameters, that are fully polymorphic. When unconstrained by a type class, their final, concrete type could be anything. 


 - **Constrained polymorphism**, on the other hand, puts type class constraints on a variable, decreasing the number of concrete types it could be, but increasing what you can do with it by defining and bringing into scope a set of specific operations.


 - **The function identity** defined in prelude is an example of **a parametric polymorphic function**. Its type variable `a` is parametrically polymorphic and not constrained by a type class. More specifically, its signture says: **for all `a`, get an argument of some type a and return a value of the same type `a`.** It is an example of maximally polymorphic signature. It allows this function to work with any type of data.

   > `id :: a -> a`


 - **If one applies id to a value of type Int, the a is fixed to type Int.** **By default, type variables are resolved at the left-most part of the type signature** and are fixed once sufficient information to bind them to a concrete type is available.


 - **A function with the type** `id :: a -> a` **cannot do anything other than return** `a`, because there is no information or method attached to its parameter at all—in other words, **since we don’t have any functions that can do anything interesting with a totally generic value, nothing can be done with** `a`. On the other hand, a function like negate, with the similar-appearing type signature of **Num a => a -> a**, constrains the a variable to be an instance of the Num type class. **In this case, `a` has fewer concrete types it could be, but there is a set of methods you can use, a set of things that can be done with a**.



 - **If a variable represents a set of possible values, then a type variable represents a set of possible types**. When there is no type class constraint, the set of possible types a variable could represent is effectively unlimited. **Type class constraints limit the set of potential types (and, thus, potential values) while also passing along the common functions that can be used with those values.**


 - **In sum, if a variable could be anything, then there’s little that can be done to it, because it has no specific methods. If it can be some types (say, a type that has an instance of Num), then it has some methods. If it is a concrete type, you lose the type flexibility but, due to the additive nature of type class inheritance, gain more potential methods.**

  - E.g. From a function f:: Integer -> Integer, we can use Num or Integral types classes functions in that function, because Integer can be an instance of Integral, and Num, is a SuperClass of Integral.


 - **A function is polymorphic when its type signature has variables that can represent more than one type.** That is, its parameters are polymorphic. Parametric polymorphism refers to fully polymorphic (unconstrained by a type class) parameters. Parametricity is the property we get from having parametric polymorphism. 


 - **Parametricity means that the behavior of a function with respect to the types of its (parametrically polymorphic) arguments is uniform. The behavior cannot change just because it was applied to an argument of a different type.**


## Polymorphic constants


- Until now, we called literal values for numbers, **polymorphic values**, a more accurate term is **polymorphic constants**. 


## Working around constraints (or Casting)


 - The following expression fails: 

    ```haskell
    λ> 6 / length [1,2,3]
    
    <interactive>:14:1: error:
        • No instance for (Fractional Int) arising from a use of ‘/’
        • In the expression: 6 / length [1, 2, 3]
          In an equation for ‘it’: it = 6 / length [1, 2, 3]
    ```

 - **The reason for this is that length isn’t polymorphic enough.** The **type class Fractional** includes several types of numbers, but **Int** isn’t one of them, and that’s all length can return.
 
   ```haskell
   λ> :t (/)
   (/) :: Fractional a => a -> a -> a -- only takes Fractional
   λ> :t length 
   length :: Foldable t => t a -> Int -- only returns Int which does not have any Fractional instance
     
   ```


 - To solve this kind of issue in haskell we can use function like `fromIntegral`. 

   ```haskell
   λ> 6 / fromIntegral (length [1,2,3])
   2.0
   λ> :t fromIntegral 
   fromIntegral :: (Integral a, Num b) => a -> b
   ```


## Type Inference


 - Haskell does not obligate us to assert a type for every expression or value in our programs, because it has type inference.


 - **Haskell will infer the most generally applicable (polymorphic) type that is correct**. 


 - Essentially, the compiler starts from the values that have types it knows and then works out the types of the other values.

## Asserting Type Signature

 - **Most of the time, we want to declare our types, rather than relying on type inference.** Adding type signatures to your code can provide guidance to you as you write your functions. **It can also help the compiler give you information about where your code is going wrong.** As programs become longer and more complex, type signatures become even more important, as they help you or other programmers trying to use your code read it and figure out what it’s supposed to do.

 
 - We can make GHCI  gear toward the type that we want inlining the type ascription. Where (1) give us the most generically applicable type, (2) is coerced toward a concrete type, here the type signature is more specific. (3) We can also coerce the type in a where Clause.

    ```haskell
    --(1) let ghc infer the type
    Prelude> triple x = x * 3 
    Prelude> :t triple 
    triple :: Num a => a -> a
    ```
   
    ```haskell
    --(2) coerce the type inference with type ascription
    λ> triple x = x * 3 :: Integer 
    λ> :t triple 
    triple :: Integer -> Integer 
    ```
    ```haskell
     --(2) coerce the type inference with type ascription in where clause
     triple x = tripleItYo x 
       where 
            tripleItYo :: Integer -> Integer 
            tripleItYo y = y * 3 
    ```
 



## Language Pragma

  - By default, when writing a module, **monomorphism restriction** applies. This means, haskell will try to default to the most concrete types, rather than the most polymorphic type applicable. More specifically, _top-level declarations by default will have a concrete type if any can be determined_.

  - In order to remove that restriction and have haskell infer the most polymorphic type applicable, a specific language pragma must be used, and place at the very to of the file, namely:

    ```haskell
    {-# LANGUAGE NoMonomorphismRestriction #-}
    ```