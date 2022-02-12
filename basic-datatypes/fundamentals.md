# Fundamentals

## Types a.k.a. Datatypes

- **Expressions**, when evaluated, reduce to values. _Every value has a type_. **Types** are how we group a set of values together that share something in common.


- **Types** play an important role in the readability, safety, and maintainability of Haskell code as they allow us to classify and delimit data, **_thus restricting the forms of data our programs can process_**.


- **Types** are also called **datatypes**


## DataType Definition

- **Data declarations** are how **datatypes** are defined in Haskell.


- **Data declarations** are composed of (i) **data constructors** that enable _**to create the values that inhabit a given type**_, (ii) and **type constructors**, which   _**denote that type**_.


- **The type constructor** is the **name of the type** and is capitalized.


- **When reading or writing type signatures** i.e the **type level** of the code, the **type names** or **type constructors** are what is used.


- **Data constructors** _makes up*_ the values that inhabit the type they are defined in. They _make up*_ the values that show up in the code, at the **term level** instead of the **type level**. By **term level**, we mean they _make up*_ the values as they appear in your code or the values that your code evaluates to.

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
   > It _coincides_ when the **Data Constructor** is **nullary** i.e. a **constant value** which is a **constant function**.  
   >   
   > **The Data Constructor type is a function type.**
   > **In other words when the Data Constructor is applied you get a value of the Type of which the Data Constructor is part of the declaration of.**





- More on Constructor inspired from [https://wiki.haskell.org/Constructor](https://wiki.haskell.org/Constructor)

    - Data constructors are not types.
    
    - They group values together and **tag** alternatives in an algebraic data type.
  
    - In other words, when **applied**, the value constructed is **tagged** by the name of the Data constructor.
  
    - In the above, there won't be any value of type Dog or Cat, there is will be value tagged as Dog or Cat.
  
    - For Instance, in `x = Dog "Goerge" `, `x` has type **Pet**, not **Dog**. However, `"Goerge"` is tagged with **Dog**.


- A Deep explanation of the notion of tag can be found here: [clarifying-data-constructor-in-haskell](https://stackoverflow.com/questions/65599830/clarifying-data-constructor-in-haskell?rq=1)


- **Type constructors** in Haskell **are not values** and can only be used in **type signatures**.


- In the above example, Pet is the type constructor. A guideline for differentiating the two kinds of constructors is that type constructors always go to the left of the `=` in a data declaration.


- **Data declarations** _always_ create a new **type constructor** but may or may not create new **data constructors**.


- **Data declarations** do not always follow precisely the same pattern — there are datatypes that use logical conjunction (and) instead of disjunction, and some type constructors and data constructors may have arguments.




- **A type alias** is a way to refer to a **type constructor** or **type constant** by an alternate name, usually to communicate something more specific or for brevity. _It is not a data declaration_ .


   > `type Name = String`



## Numeric Datatypes:


- We have the datatypes **Int**, **Word**, **Integer**, which haskell refers to as **Integral**, and the datatypes **Rational**, **Double**, **Fixed**, **Float**, **Scientific** which haskell refer to as **Fractional**.  It is important to note however that **Integral** and **Fractional** are not _datatypes_. They are _Types Classes_ that (in short) group together the operations common to those groups of _datatypes_. Both **Fractional** and **Integral** are themselves **Num**. **Num** groups the operation common to all the _numeric datatypes_. 

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

  
- In the above, `Fractional a =>` can be red as  _**with the constraint of having a Fractional instance for type a we have ...**_ Another way to read it can be, _**with the constraint that type a implements the Fractional Type Class, we have ...**_


- In Haskell we say that **Num** is a **Super Class** of **Integral** and **Fractional**. Note this has nothing to do with the _object-oriented paradigm_. Here **Super Class** is because the Fractional and Integral **type class constraint**, state that the types that implement them, must already implement the Num type class. **It is a constraint dependency !**

  > `λ> :i Fractional`  
  > `type Fractional :: * -> Constraint`    
  > `class Num a => Fractional a where `   

  > `λ> :i Integral`  
  > `type Integral :: * -> Constraint`  
  > `class Num a => Integral a where`  

- The **literal values** of numbers are _**polymorphic values**_, meaning they can be of different types depending on the context or type annotation.

   > `λ> :t 2`  
   > `2 :: Num p => p`  
  
   > `λ> :t 2::Double`  
   > `2::Double :: Double`  
  
   > `λ> :t 2.0`  
   > `2.0 :: Fractional p => p`  

- It is critical to note that when the type is unspecified, haskell pick the one that satisfies the context, and the _heuristic_ is to go for _the most generic type description (i.e. concrete type or type class constraint)_. This means the _type class constraint_, can be enough to describe the _**polymorphic value**_ rather than the _concrete datatypes_.

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
   > `λ> x /= 5`  
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


- Note that this is leaning on the Ord type class instances for the **List** and **Char** type. You can only compare lists of items where the items themselves also have an instance of Ord. 

    ```haskell
    instance (Eq a) => Eq [a] where   
        {-# SPECIALISE instance Eq [[Char]] #-}  
        {-# SPECIALISE instance Eq [Char] #-}  
        {-# SPECIALISE instance Eq [Int] #-}   
         []     == []     = True   
         (x:xs) == (y:ys) = x == y && xs == ys   
         _xs    == _ys    = False
    ```    

- The code above taken from _GCH.prim.Classes.sh_ states in the first line that the `Eq [a]` instance is defined with the constrained that (Eq a) exists. 



## Tuples



- **Tuples in Haskell** are the usual means of briefly carrying around multiple values without giving that combination its own name. That is, is allows to store and pass around multiple values within a single, composite value, where the type of that value is tuple rather than a user define type. 


- **A tuple** is an ordered grouping of values. 


- **The types** of the elements of **tuples** are allowed to vary, so both `(String, String)` and `(Integer, String)` are valid **tuple types**.


- **Tuple have a fixed number of constituents**. We refer to tuples by the number of values in each tuple: the 2-tuple or pair, for example, has two values inside it, (x, y); the 3-tuple or triple has three, (x, y, z), and so on. This number is also known as the **tuple’s arity**.
  

- **There is no tuple with only one element** in Haskell,  but there is a **“zero” tuple**, also called _**unit**_ or _**()**_.


- **Tuples have a distinctive, built-in syntax that is used at both type and term levels.**
  
    > `λ> :i (,)`   
    > `data (,) a b = (,) a b`   


- **The type Constructor of Tuple `(,) a b`** has two parameters, represented by the **type variables** **a** and **b**. Those have to be applied to concrete types, much as variables at the term level have to be applied to values in order to evaluate a function. It is a *parameterized type constructor*, as opposed to a *type constant* such as Bool. That is, the **type variables** makes it a polymorphic type constructor, or polymorphic type in short. In other words, the **Tuple Type** give rise to multiple concrete type upon application. e.g. `(String, String)` or `(String, Int)` are concrete Tuple Type. 


- **The Data Constructor of Tuple `(,) a b`** has two parameters, represented by **the variables of type a and b**. That is, this is the term level. In other worlds, when a Data constructor is Parameterized, the Parameters are Values of which the types is provided as Parameters. 


- **Where the Type constructor is applied to Types (i.e. a and b), the Data Constructor is applied to Values of type a and b.** **The types of those values are the parameters of the Data Constructor**. Here, the first one has **type a**, so it's just a **value** of the **parameter type a** introduced in the type Constructor.


- **The Signature of a Data constructor that takes arguments i.e. values, is the type of those values !!**


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


- **Lists have their own distinct [] syntax**. Like the tuple syntax, it is used for both (i) the **type constructor** in _type signatures_, and (ii) the **Data Constructor** at the _term level_ to express list values. 


- **The number of values** that will be in the list isn’t specified in the type—unlike tuples, where the _arity_ is set in the type and is _immutable_.





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

- **Arity** is the number of arguments a function accepts. This notion is a little slippery in Haskell since, due to **currying**, all functions are **arity one** (or _“unary”_), and we handle accepting multiple arguments by nesting functions.


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





