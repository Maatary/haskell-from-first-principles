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

- We use the expression _makes up_* to highlight the fact that although in colloquial haskell, Data  constructor are referred as the _values_ that inhabit the _types_ of the Data declaration, **technically** they are not, but actually are the _means_ to makes those value. The conflation comes from the fact that the value actually created, is a value **"tagged"** by the name of the Data constructor (_more explanation  below_). 


- **Data constructors** themselves have a type and can either be constant values _(nullary)_ or _take one or more arguments_, like functions.

   > The data constructors above have the following types:  
   >
   > `Prelude> :t Cat`   
   > `Cat :: Pet `  
   > `Prelude> :t Dog`    
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
  
    - For Instance, in ** `x = Dog "Goerge" ` ** `x` has type **Pet**, not **Dog**. However, `"Goerge"` is tagged with **Dog**.


- A Deep explanation of the notion of tag can be found here: [clarifying-data-constructor-in-haskell](https://stackoverflow.com/questions/65599830/clarifying-data-constructor-in-haskell?rq=1)


- **Type constructors** in Haskell **are not values** and can only be used in **type signatures**.


- In the above example, Pet is the type constructor. A guideline for differentiating the two kinds of constructors is that type constructors always go to the left of the `=` in a data declaration.


- **Data declarations** _always_ create a new **type constructor** but may or may not create new **data constructors**.


- **Data declarations** do not always follow precisely the same pattern — there are datatypes that use logical conjunction (and) instead of disjunction, and some type constructors and data constructors may have arguments.




- **A type alias** is a way to refer to a **type constructor** or **type constant** by an alternate name, usually to communicate something more specific or for brevity. _It is not a data declaration_ .


   > `type Name = String`



## Numeric Datatypes:


- We have **Int**, **Word**, **Integer**, which haskell refers to as **Integral**, and **Rational**, **Double**, **Fixed**, **Float**, **Scientific** which haskell refer to as Fractional.  It is important to note however that **Integral** and **Fractional** are not _datatypes_. They are _Types Classes_ that (in short) group together the operations common to those groups of _datatypes_. Both **Fractional** and **Integral** are themselves **Num**. **Num** groups the operation common to all the _numeric datatypes_. 


- Integral and Fractional are type classes. They represent operation on respectively all Integral datatype (Int, Integer, ...) and Fractional datatype(Float, Double, ....). Integral and Fractional are not datatypes.


- In `(/) :: Fractional a => a -> a -> a` , `Fractional a =>` can be red as  _**with the constraint of having a Fractional instance for type a we have ...**_


- The _literal values_ of numbers are _polymorphic values_, meaning they can be of different types depending on the context or type annotation.


- It is critical to note that when the type is unspecified, haskell pick the one that satisfies the context, and the _heuristic_ is to go for _the most generic type description (i.e. concrete type or type class constraint)_. This means the _type class constraint_, can be enough to describe the polymorphic value rather than the _concrete datatypes_.


## Type Classes

- **A type class** is a set of operations defined with respect to a polymorphic type. When a type has an instance of a type class, values of that type can be used in the standard operations defined for that type class.


- **In Haskell,** **type classes** are _unique pairings_ of a _class_ and a _concrete instance_. This means that if a given type `a` has an instance of `Eq`, it has only one instance of `Eq`.


## Tuples

- **A tuple** is an ordered grouping of values. In Haskell, you cannot have a tuple with only one element, but there is a “zero” tuple, also called _unit_ or _()_.


- **The types** of the elements of **tuples** are allowed to vary, so both `(String, String)` and `(Integer, String)` are valid **tuple types**.


- **Tuples in Haskell** are the usual means of briefly carrying around multiple values without giving that combination its own name.


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





