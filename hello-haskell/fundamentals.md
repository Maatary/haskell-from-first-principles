# Fundamentals


## Expressions


- Everything in Haskell is an _expression_ or _declaration_, where an expression is a well-structured combination of constants, variables, and functions.


- **Expressions** may be values, combinations of values, and/or functions applied to values. Expressions evaluate to a result.


- **Expressions** are the building blocks of our programs, and programs themselves are one big expression made up of smaller expressions.


- **Declarations** are toplevel bindings that allow us to name expressions.


- We say that expressions are in _normal form_ when there are no more evaluation steps that can be taken, or, put differently, when they’ve reached an _irreducible form_.

- **Reducible expressions** are also called _redexes_.


- In Haskell, an expression is 


**Note:** The terms _**argument**_ and _**parameter**_ are often used interchangeably. However, it is worthwhile to understand the distinction. A _**parameter**_, or _**formal parameter**_, represents a value that will be passed to a function when that function is called. Thus, _**parameters** are usually variables_. An **argument** is an input value the function is applied to. _A function’s parameter is bound to an argument when the function is applied to that argument._

(Page 64).


## Functions


- **Function** is a specific type of _expression_


- **A function** is an _expression_ that is applied to _an argument_ and always returns a result. 


- Because _they are built purely of expressions_, they will always evaluate to the _same result_ when given the _same values_.



- As in the _lambda calculus_, all _functions_ in Haskell **_take one argument and return one result_**. 


- In Haskell, when it seems like we are passing multiple arguments to a function, we are actually applying _a series of nested functions_, each to one argument. This is called **_currying_**.


- **Functions** are how we factor out a pattern into something we can reuse with different inputs.


- **Functions** can also appear in the _expressions that form the bodies of other functions_ or be used as _arguments to functions_, just as any other value can be.


- **Value** are just irreducible expression.


## Evaluation


- **Evaluating an expression**, is reducing the _terms_ until the expression reaches its _simplest form_. 


- Once a _term_ has reached its _simplest form_, we say that it is irreducible or _finished evaluating_. 


- An _expression_ in its _simplest form_, is what we usually called a **_value_**.


- Haskell uses a **_non-strict evaluation_** (sometimes called “_**lazy evaluation**_”) **_strategy_** that _defers_ evaluation of terms until they’re forced to evaluate by other terms that refer to them.


- **Values** are _irreducible_, but **applications of functions to arguments** are _reducible_.


- As in the _lambda calculus_, _application is evaluation_: applying a function to an argument allows evaluation or reduction.


- **Values** are expressions, but they cannot be reduced further. Values, in other words, are a _terminal point of reduction_.


## Let and Where

- The difference is that **let** introduces an _expression_, so it can be used wherever you can have an _expression_, but **where** is a _declaration_ and _is bound to a surrounding syntactic construct_.






 
