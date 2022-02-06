# Fundamentals


## Reflections:

- Integral and Fractional are type classes. They represent operation on respectively all Integral datatype (Int, Integer, ...) and Fractional datatype(Float, Double, ....). Integral and Fractional are not datatypes.


- In `(/) :: Fractional a => a -> a -> a` , `Fractional a =>` can be red as _with constraint Fractional a we have_  or  _with Instance Fractional a we have_ , or more verbose, _**with the constraint of having a Franctional instance for type a**_



## Definitions

## Tuples

- **A tuple** is an ordered grouping of values. In Haskell, you cannot have a tuple with only one element, but there is a “zero” tuple, also called _unit_ or _()_.


- **The types** of the elements of **tuples** are allowed to vary, so both `(String, String)` and `(Integer, String)` are valid **tuple types**.


- **Tuples in Haskell** are the usual means of briefly carrying around multiple values without giving that combination its own name.
 
## Type Classes

- **A type class** is a set of operations defined with respect to a polymorphic type. When a type has an instance of a type class, values of that type can be used in the standard operations defined for that type class.


- **In Haskell,** **type classes** are _unique pairings_ of a _class_ and a _concrete instance_. This means that if a given type `a` has an instance of `Eq`, it has only one instance of `Eq`.


## DataType Definition

- **Data declarations** define new **datatypes** in Haskell. 


- **Data declarations** are composed of (i) **data constructors** that enable _**to create the values that inhabit a given type**_, (ii) and **type constructors**, which   _**denote that type**_.


- **Data declarations** _always_ create a new **type constructor** but may or may not create new **data constructors**. 


- **Data constructors** themselves have a type and can either be constant values _(nullary)_ or _take one or more arguments_, like functions.


   > **Example**
   > 
   > `type Name = String`  
   > `data Pet = Cat | Dog Name`  
   >
   > The data constructors have the following types:
   >
   > `Prelude> :t Cat`   
   > `Cat :: Pet `  
   > `Prelude> :t Dog`  
   > `Dog :: Name -> Pet`  
   >    
   > Here `Dog :: Name -> Pet` really highlight what is meant by the type of the **Data Constructor**.  
   > It is not to be confused with the type of the value being constructed of the **Data Constructor**.  
   > It _coincides_ when the **Data Constructor** is **nullary** i.e. a **constant value** which is a **constant function**. 
   > 
   > **The Data Constructor type is a function type** 



- **Type constructors** in Haskell **are not values** and can only be used in **type signatures**. 
 

- In the above example, Pet is the type constructor. A guideline for differentiating the two kinds of constructors is that type constructors always go to the left of the = in a data declaration.


- **A type alias** is a way to refer to a **type constructor** or **type constant** by an alternate name, usually to communicate something more specific or for brevity. _It is not a data declaration_ .


  > `type Name = String`  


## Functions Arity & Currying 

- **Arity** is the number of arguments a function accepts. This notion is a little slippery in Haskell since, due to **currying**, all functions are **arity one** (or _“unary”_), and we handle accepting multiple arguments by nesting functions.

 

## Polymorphism

- **Polymorphism** means being able to write code in terms of **values** that may be **one of several types** or **of any type**.


- **Polymorphism** in Haskell, is either **parametric** or **constrained**. 


- The identity function, `id`, is an example of a parametrically polymorphic function: 


   > `id :: a -> a`  
   > `id x = x` 


- Here, `id` works for _a value of any type_, because it doesn't use any information specific to a given type or set of types. 


- The following function, isEqual, is polymorphic, but **constrained** or **bounded** to the set of types that have an instance of the Eq type class: 


   > isEqual :: Eq a => a -> a -> Bool  
   > isEqual x y = x == y  


