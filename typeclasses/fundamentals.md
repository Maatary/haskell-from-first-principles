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













