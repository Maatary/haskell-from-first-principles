# Fundamentals

## Types a.k.a. Datatypes

- **Types**, also called **datatypes**, provide the means to build programs more quickly and also allow for greater ease of maintenance.


- **Types** play an important role in the readability, safety, and maintainability of Haskell code as they allow us to classify and delimit data, **_thus restricting the forms of data our programs can process_**.


- **Expressions**, when evaluated, reduce to values. _Every value has a type_. **Types** are how we group a set of values together that share something in common.


 - **Types** can be though of as **set** in mathematics.


## DataType Definition

- **Data declarations** are how **datatypes** are defined in Haskell.


- **Data declarations** are composed of (i) **data constructors** that enable _**to create the values that inhabit a given type**_, (ii) and **type constructors**, which   _**denote that type**_.


- **The type constructor** is the **name of the type** and is capitalized.


- **When reading or writing type signatures** i.e the **type level** of the code, the **type names** or **type constructors** are what is used.


- **Data constructors** _makes up*_ the values that inhabit the type they are defined in. They _make up*_ the values that show up in the code, at the **term level** instead of the **type level**. By **term level**, we mean they _make up*_ the values as they appear in the code or the values that the code evaluates to.

   > **Example of data declaration**
   >
   > `type Name = String`  
   > `data Pet = Cat | Dog Name`
  
  
- In the above example there are two **data constructors** and the `|` denote a _disjunction_, which makes the datatype a **Sum type**. That is, a Pet is either a Cat or a Dog. 


- We use the expression _makes up_* to highlight the fact that although in colloquial haskell, Data  constructor are referred as the _values_ that inhabit the _types_ of the Data declaration, **technically** they are not, but actually are the _means_ to makes those value. The conflation comes from the fact that the value actually created, is a value **"tagged"** by the name of the Data constructor (_more explanation  below_). 


- **Data constructors** themselves have a type and can either be constant values _(nullary)_ or _take one or more arguments_, like functions.

   > The data constructors above have the following types:  
   >
   > `λ> :t Cat`   
   > `Cat :: Pet `  
   > `λ> :t Dog`    
   > `Dog :: Name -> Pet`  
   >
   > Here `Dog :: Name -> Pet` really highlight what is meant by the type of the **Data Constructor**.  
   > It is not to be confused with the type of the value being constructed out of the **Data Constructor**.  
   > The Data constructor and the value that it produces _coincides_ when the **Data Constructor** is **nullary** i.e. a **constant value** which is a **constant function**.  
   >   
   > **The Data Constructor type is a function type.**
   > **In other words when the Data Constructor is applied you get a value of the Type of which the Data Constructor is part of the declaration of.**





- More on Constructor inspired from [https://wiki.haskell.org/Constructor](https://wiki.haskell.org/Constructor)

    - Data constructors are not types.
    
    - They group values together and **tag** alternatives in an algebraic data type.
  
    - In other words, when **applied**, the value constructed is **tagged** by the name of the Data constructor.
  
    - In the above, there won't be any value of type Dog or Cat, there is will be value tagged as Dog or Cat.
  
    - For Instance, in `x = Dog "Goerge" `, `x` has type **Pet**, not **Dog**. However, `"Goerge"` is tagged with **Dog**. `Dog "Goerge"` is the value.


- A Deep explanation of the notion of tag can be found here:

  - [clarifying data constructor in haskell](https://stackoverflow.com/questions/65599830/clarifying-data-constructor-in-haskell?rq=1)
  
  - [Algebraic Data Types - Explanation](https://en.wikipedia.org/wiki/Algebraic_data_type) (The explanation section goes particularly at length to explain the notion of tag)
  
  - [what do haskell data-constructors construct](https://stackoverflow.com/questions/51509949/what-do-haskell-data-constructors-construct)
  
  - [https://wiki.haskell.org/Algebraic_data_type](https://wiki.haskell.org/Algebraic_data_type)


- **Type constructors** in Haskell **are not values** and can only be used in **type signatures**.


- In the above example, Pet is the type constructor. A guideline for differentiating the two kinds of constructors is that type constructors always go to the left of the `=` in a data declaration.


- **Data declarations** _always_ create a new **type constructor** but may or may not create new **data constructors**.


- **Data declarations** do not always follow precisely the same pattern — there are datatypes that use logical conjunction (and) instead of disjunction, and some type constructors and data constructors may have arguments.


- **A type alias** is a way to refer to a **type constructor** or **type constant** by an alternate name, usually to communicate something more specific or for brevity. _It is not a data declaration_ .

    > `type Name = String`


- Another example of data type definition

    > `data Bool = False | True`
    > 
    > `Bool` is the **Type Constructor** a.k.a the **Type Name**
    > 
    > `False` and `True` are the two  **Nullary Data Constructors** and therfore the two possible values i.e. inhabitant of the type **Bool**.



## Numeric Datatypes:


- We have the datatypes **Int**, **Word**, **Integer**, which haskell refers to as **Integral**, and the datatypes **Rational**, **Double**, **Fixed**, **Float**, **Scientific** which haskell refer to as **Fractional**.  It is important to note however that **Integral** and **Fractional** are not **datatypes**. They are **Types Classes**, i.e. **Classes of Types**. In short, it means that they group together the **operations common** to those groups of **datatypes**. Both **Fractional** and **Integral** are themselves **Num**. **Num** groups the operation common to all the **numeric datatypes**. 


- The reason and implication of the existence of each datatype within a same category such as Int, Word and Integer of the Integral category (type class) is mostly related to expressiveness and performance issue. More can be found in **page 93 of this chapter** and in the following link https://stackoverflow.com/questions/1184296/why-can-haskell-handle-very-large-numbers-easily


- **Types Classes** semantics and mechanics are thoroughly studied in their [dedicated chapter](../typeclasses/fundamentals.md). At this point, we could at least clarify that when we said above that groups of **datatypes** are referred as **Integral** or **Fractional**, it means that the DataTypes in those groups respectively have **an instance of the Integral Type Class** or **instance of the Fractional Type Class**, and all of them have instances of the **Num Type Class**. An overview of this is provided below.

    ```haskell
    λ> :i Integer
    type Integer :: * -- means Integer is a concrete Type (more on this when we get to the Type chapter)
    data Integer
      = integer-gmp-1.0.3.0:GHC.Integer.Type.S# ghc-prim-0.6.1:GHC.Prim.Int#
      | integer-gmp-1.0.3.0:GHC.Integer.Type.Jp# {-# UNPACK #-}integer-gmp-1.0.3.0:GHC.Integer.Type.BigNat
      | integer-gmp-1.0.3.0:GHC.Integer.Type.Jn# {-# UNPACK #-}integer-gmp-1.0.3.0:GHC.Integer.Type.BigNat
            -- Defined in ‘integer-gmp-1.0.3.0:GHC.Integer.Type’
    instance Integral Integer -- Defined in ‘GHC.Real’
    instance Num Integer      -- Defined in ‘GHC.Num’
    ```
  
    ```haskell
    λ> :i Double
    type Double :: * -- means Integer is a concrete Type (more on this when we get to the Type chapter)
    data Double
      = ghc-prim-0.6.1:GHC.Types.D# ghc-prim-0.6.1:GHC.Prim.Double#
            -- Defined in ‘ghc-prim-0.6.1:GHC.Types’
    instance Num Double -- Defined in ‘GHC.Float’
    instance Fractional Double -- Defined in ‘GHC.Float’
    ```

### Num, Integral & Fractional Type Classes

- Num, Integral and Fractional are **type classes**. Num, represents operations common to all numeric datatypes, while Integral and Fractional respectively represent operation specific but common to all Integral datatype (Int, Integer, ...) and Fractional datatype (Float, Double, ....) e.g. `(+)` `(-)` `(*)` `(/)` `div` ...

   > Example of Function found in the Num Type Class
   >
   > `λ> :t (+)`
   >
   > `(+) :: Num a => a -> a -> a`
  
   > Example of Function found in the Fractional Type Class
   > 
   > `λ> :t (/)` 
   > 
   > `(/) :: Fractional a => a -> a -> a`

   > Example of Function found in the Integral Type Class
   >
   > `λ> :t div`
   >
   > `div :: Integral a => a -> a -> a`

  
- In the above, `Fractional a =>` can be red as _**"with the constraint of having a Fractional Instance for the type `a` we have ..."**_ or more commonly, _**"with the constraint that type `a` implements the Fractional Type Class, we have ..."**_ That is, there must be a declaration of how the operations from that **type class** will work for that **particular type**.




- In Haskell we say that **Num** is a **Super Class** of **Integral** and **Fractional**. Note this has nothing to do with the _object-oriented paradigm_. Here **Super Class** is because the Fractional and Integral **type class constraint**, state that the types that implement them, must already implement the Num type class. **It is a constraint dependency !** 

  > `λ> :i Fractional`  
  > 
  > `type Fractional :: * -> Constraint` -- Denote the **Type** of Type class, which will be explained in later chapters  
  > 
  > `class Num a => Fractional a where `  -- Begin Type Class definition  

  > `λ> :i Integral`  
  > 
  > `type Integral :: * -> Constraint`  -- Denote the **Type** of Type class   which will be explained in later chapters  
  > 
  > `class Num a => Integral a where`  -- Begin Type Clas Definition    

- The **literal values** of numbers are _**polymorphic values**_, meaning they can be of different types depending on the context or type annotation.

    > `λ> :t 2`      
    > 
    > `2 :: Num p => p`  
    >  
    >  The literal 2 is of type `p` where `p` must implement the Num Type Class
  
    > `λ> :t 2::Double`
    >   
    > `2::Double :: Double`  
    >  
    >  The literal 2 is of type `Double`. 
    > 
    >  The ::Double force the instantiation of the polymorphic type `Num p => p` to the concrete type Double. It provides for context.
  
    > `λ> :t 2.0`  
    > 
    > `2.0 :: Fractional p => p` 
    >   
    > The literal 2 is of type  `p` where `p` must implement the Fractional Type Class
  

- Numeric literals are made polymorphic by design in haskell. More specifically, any numeric literal value that is without a dot in it, negative or positive is automatically desiguarded into the application of the function `fromInterger`, and any numeric literal that is with a dot in it is desiguarded into the application of the function `fromRational`

    ```haskell
    λ> :t fromInteger
    fromInteger :: Num a => Integer -> a
  
    λ> :t fromRational 
    fromRational :: Fractional a => Rational -> a
    ```
    Hence

    > `42`  
    >
    > desiguard into
    > 
    > `fromInteger (42::Integer)`  
    > 
    > which results into
    >
    > 42 :: Num p => p

    as witnessed in GHCI

    ```haskell
    λ> :t 42
    42 :: Num p => p
    ```

- Numeric literals are defined in this indirect way so that they may be interpreted as values of any appropriate numeric type according to the context.


- Note that haskell uses `fromInterger` because **Integer** is the widest type of the integral category of number. Likewise, it uses `fromRational` because **Rational** is the widest type of the fractional category of number. 


- The detail of what is meant by "so they may be interpreted as values of any appropriate numeric type according to the context" goes beyond the scope of this chapter. We showed a simplified example of it in ``2::Double :: Double`` where the Double at the end provide for a context.  It will be thoroughly explained in later chapters. Meanwhile, some references are provided below that can help start understanding the underlying mechanic including the official specification.


  - https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1360006.4.1
  
  - https://www.reddit.com/r/haskell/comments/iblo44/is_int_in_haskell_an_algebraic_data_type/

  - https://www.reddit.com/r/haskell/comments/ptzzwg/diehls_comments_on_haskell_numbers_confuse/
  
  - https://stackoverflow.com/questions/73935193/clarifying-numeric-literal-definition-in-haskell 
  
  - https://kseo.github.io/posts/2017-01-04-type-defaulting-in-haskell.html
  
  - https://stackoverflow.com/questions/71196780/what-instance-of-num-is-used-with-literals-e-g-in-print-1
  
  - https://stackoverflow.com/questions/64139418/do-i-understand-this-haskell-code-with-fromintegral-correctly


  - https://eli.thegreenplace.net/2018/return-type-polymorphism-in-haskell/
  - https://stackoverflow.com/questions/74028227/why-is-frominteger-allowed-to-return-a-polymorphic-value-but-that-does-not/74028487#74028487
  - https://chrisdone.com/posts/value-polymorphism/
  - https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#type-defaulting-in-ghci
  - https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/interactive-evaluation.html (type-defaulting-in-ghci)
  - https://andrew.gibiansky.com/blog/haskell/haskell-typeclasses/ (Literals - Some tripping point about literal)
  - https://diogocastro.com/blog/2018/06/17/typeclasses-in-perspective/#return-type-polymorphism



### Eq & Ord Type Classes

- We can compare values with the following infix Operators:

   > `λ> x = 5`  
   > 
   > `λ> x == 5`  
   > `True`  
   > 
   > `λ> x == 6`  
   > `False`  
   > 
   > `λ> x < 7`  
   > `True` 
   > 
   > `λ> x > 3`  
   > `False`
   > 
   > `λ> x == 5`  
   > `True`
   > 
   > `λ> x /= 5`   -- not equal test
   > `False` 


 - Following on the above, looking at the type of the infix operator we get:  
 
   > `λ> :t (==)`  
   > `(==) :: Eq a => a -> a -> Bool`   
   > 
   > `λ> :t (<)`   
   > `(<) :: Ord a => a -> a -> Bool`


- Like operation on **Numeric** e.g. `(+)` `(/)` given by **Integral/Fractional/Num**, comparison operators are provided by the  **Eq** and **Ord** _Types Classes_. However, it is worth noticing the _datatypes_ that _implement_ (_i.e. have instances of_) these _Types Classes_ go beyond **Numerics**. It is anything that can be _compared_ or _ordered_. 

   > `λ> 'a' == 'b'`  
   >  `False`
   > 
   > `λ> "Julie" == "Chris"`  
   > `False`  
   >  
   > `λ> "Julie" == "Chris" False`  
   > 
   > `λ> ['c', 'b'] > ['a', 'b']`   
   > `True`


- Note that the comparison of **List of Char above** (i.e. `[Char]`) is **leaning** on the Ord type class instances for the **List** and **Char** type. You can only compare lists of items where the items themselves also have an instance of Ord. This will be thoroughly studied in the [chapter on Type classes](../typeclasses/fundamentals.md)

    ```haskell
    instance (Eq a) => Eq [a] where   -- An Instance for `Eq [a]` requires that there is an Instance `Eq a`
        {-# SPECIALISE instance Eq [[Char]] #-}  
        {-# SPECIALISE instance Eq [Char] #-}  
        {-# SPECIALISE instance Eq [Int] #-}   
         []     == []     = True   
         (x:xs) == (y:ys) = x == y && xs == ys   
         _xs    == _ys    = False
    ```    

- The code above taken from _GCH.prim.Classes.sh_ states in the first line that the `Eq [a]` instance is defined with the constrained that (Eq a) exists. 



## Tuples



- **Tuples in Haskell** are the usual means of briefly carrying around multiple values without giving that combination its own name. That is, it allows to store and pass around multiple values within a single, composite value, where the type of that value is tuple rather than a user define type. 


- **A tuple** is an ordered grouping of values. 


- **The types** of the elements of **tuples** are allowed to vary, so both `(String, String)` and `(Integer, String)` are valid **tuple types**.


- **Tuple have a fixed number of constituents**. We refer to tuples by the number of values in each tuple: the 2-tuple or pair, for example, has two values inside it, (x, y); the 3-tuple or triple has three, (x, y, z), and so on. This number is also known as the **tuple’s arity**.
  

- **There is no tuple with only one element** in Haskell,  but there is a **“zero” tuple**, also called _**unit**_ or _**()**_.


- **Tuples have a distinctive, built-in syntax that is used at both type and term levels.**
  
   > `λ> :i (,)`   
   > `data (,) a b = (,) a b`   


- **The type Constructor of Tuple `(,) a b`** has two parameters, represented by the **type variables** **a** and **b**. Those have to be applied to concrete types, much as variables at the term level have to be applied to values in order to evaluate a function. It is a *parameterized type constructor*, as opposed to a *type constant* such as Bool. That is, the **type variables** makes it a polymorphic type constructor, or polymorphic type in short. In other words, the **Tuple Type** give rise to multiple concrete type upon application. e.g. `(String, String)` or `(String, Int)` are concrete Tuple Type. 


- **The Data Constructor of Tuple `(,) a b`** has two parameters, represented by **the variables of type a and b**. That is, this is the term level. In other worlds, when a Data constructor is Parameterized, the Parameters are Values of which the types is provided as Parameters***(not that accurate). 


- **Where the Type constructor is applied to Types (i.e. a and b), the Data Constructor is applied to Values of type a and b.** **The types of those values are the parameters of the Data Constructor**. Here, the first one has **type a**, so it's just a **value** of the **parameter type a** introduced in the type Constructor.


- **For a parameterized Data constructor, that is, that takes arguments i.e. values, the parameters are represented by the type of those values !!** The point being that, a Data Constructor Definition is a **Signature**. It is not a function. The actual function is automatically synthesize by the compiler.


- **The Tuple Data Constructor is a Product Type**, **not a Sum Type**. A **product type** represents a logical conjunction: you must supply both arguments to construct a value. 


- **The two type variables are different**, so that allows for tuples that contain values of two different types. _The types are not, however, required to be different_.

    ```haskell
    λ> (,) 8 "juile" -- classic data constructor syntax
    (8,"juile")
  
    λ> (8, "julie")  -- same as above but with special syntax
    (8,"julie")
  
    λ> :t (8, "julie")
    (8, "julie") :: Num a => (a, [Char]) -- the most generic type for 8 is that it implements the Num type class
    ```


## List

- **Lists** are another type used to contain multiple values within a single value. 


- **All the elements** of a list must be of the **same type**. 


- **Lists have their own distinct `[]` syntax**. Like the tuple syntax, it is used for both (i) the **type constructor** in _type signatures_, and (ii) the **Data Constructor** at the _term level_ to express list values. 


- **The number of values** that will be in the list isn’t specified in the type—unlike tuples, where the _arity_ is set in the type and is _immutable_.


- The following provide a quick glimpse at the List datatype, which will be studied in details in his own chapter. 

   > The List DataType
   >
   > `λ> :i []`   
   > `data [] a = [] | a : [a]`  

   > List Data Constructor Type Signature  
   > 
   > `λ> :t (:)`   
   > `(:) :: a -> [a] -> [a]`  
   > 
   > `λ> :t []`  
   > `[] :: [a]`  

    ```haskell
    λ> p = "Papuchon" 
    λ> awesome = [p, "curry", ":)"] 
    λ> awesome 
    λ> ["Papuchon","curry",":)"] 
    λ> :t awesome 
    λ> awesome :: [[Char]]
  
    λ> "Maat" : ["Papuchon","curry",":)"] -- Use the `:` infix data constructor 
    λ> ["Maat","Papuchon","curry",":)"]
    λ> "Deamon" : [] -- Use the `:` infix data constructor & the `[]` nullary data constructor
    λ> ["Deamon"]
    λ> ["Deamon"] ++  ["Maat","Papuchon","curry",":)"]
    λ> ["Deamon","Maat","Papuchon","curry",":)"]
    λ> :t (++)
    λ> (++) :: [a] -> [a] -> [a]
    ```

- Things like the special syntax that allows to create a list by the just writing `[1,2,3]` or  `["Papuchon","curry",":)"]` will  be further address in the List chapter. 


## Type Classes

- **A type class** is a set of operations defined with respect to a polymorphic type. When a type has an instance of a type class, values of that type can be used in the standard operations defined for that type class.


- **In Haskell,** **type classes** are _unique pairings_ of a _class_ and a _concrete instance_. This means that if a given type `a` has an instance of `Eq`, it has only one instance of `Eq`.


## If-Then-Else Expression

   > if CONDITION   
   > then EXPRESSION_A   
   > else EXPRESSION_B  

- If the condition (_which must evaluate to Bool_) **reduces** to the value True, then EXPRESSION_A is the result, otherwise it’s EXPRESSION_B.


- The types of the expressions in the then and else clauses must be the same



## Functions Arity & Currying 

- **Arity** is the number of arguments a function accepts. This notion is a little slippery in Haskell since, due to **currying**, all functions are **arity one** (or _“unary”_), and we handle accepting multiple arguments by nesting functions. In other words, when we write

    > f x y z = x + y + z  

    It is desiguarded by the compiler to
  
    > f = \x -> \y -> \z -> x + y + z

    Which is essentially its pure Lambda-Calculus form.


## Polymorphism

- **Polymorphism** means being able to write code in terms of **values** that may be **one of several types** or **of any type**.


- **Polymorphism** in Haskell, is either **parametric** or **constrained**. 


- **The identity function**, `id`, is an example of a parametrically polymorphic function: 


   > `id :: a -> a`  
   > `id x = x` 


- Here, `id` works for _a value of any type_, because it doesn't use any information specific to a given type or set of types. 


- The following function, `isEqual`, is **polymorphic**, but **constrained** or **bounded** to the set of types that have an instance of the Eq type class: 


   > isEqual :: Eq a => a -> a -> Bool  
   > isEqual x y = x == y  


## Noteworthy

- A function whose body is not defined yet can be written as such. 

   > `f x = undefined`


## Names

- **In Haskell, there are seven categories of entities that have names**: functions, term-level variables, data constructors, type variables, type constructors, type classes, and modules. 


- **Term-level variables**, **Data Constructors** and **functions** exist in your terms. **Term level** is where your values live and is the code that executes when your program is running.


- At the **Type level**, which is used during the static analysis and verification of your program, we have **type variables**, **type constructors**, and **type classes**.


- Lastly, for the purpose of organizing our code into coherent groupings across different files, we have **modules**.


## Variables Naming Convention

 - **Type variables** (that is, variables in type signatures) generally start at **a** and go from there: **a, b, c,** and so forth. You may sometimes see them with numbers appended to them, e.g., **a1**.


 - **Functions** can be used as arguments and in that case are typically labeled with variables starting at **f** (followed by **g** and so on). They may sometimes have numbers appended (e.g., **f1**) and may also sometimes be decorated with the **' character**, as in **f'** ( This would be pronounced _"eff-prime"_)


 - _**Variables do not have to be a single letter**. In small programs, they often are; in larger programs, they usually should not be a single letter. If there are many variables in a function or program, as is common, it is helpful to have descriptive variable names. It is often advisable in domain-specific code to use domain-specific variable names._


 - **Arguments to functions** are most often given names starting at **x**, again occasionally seen numbered as in **x1**. Other single-letter variable names may be chosen when they serve a mnemonic role, such as choosing r to represent a value that is the radius of a circle. 

   - If you have a list of items, and you have used the name **x** to refer to one such item, by convention the list itself will usually be called **xs**, that is, **the plural of x**. You will often see this convention in the form **(x:xs)**, _which means you have a list where the “head,” or first element, is x, and the rest of the list, or “tail,” is xs_.

 




 