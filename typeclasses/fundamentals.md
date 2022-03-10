# Fundamentals

## What are Type Classes ?

 - **Type classes** and **types** in Haskell **are, in a sense, opposites**. **Whereas a declaration of a type defines how that type in particular is created**, **a declaration of a type class defines how a set of types are consumed or used in computations**.


 - **This tension is related to the _expression problem_**, which is about defining code in terms of **_how data is created_** or **_processed_**.


 - **As Philip Wadler put it,** "_The goal is to define a datatype by cases, where one can add new cases to the datatype and new functions over the datatype, without recompiling existing code, and while retaining static type safety (e.g., no casts)_".


 - **It may help to think of type classes as being like interfaces to data that can work across multiple datatypes**. The latter facility is why type classes are a means of **ad hoc polymorphism** — **ad hoc because type class code is dispatched by type**.




