# Fundamentals


## Expressions


- Everything in Haskell is an **_expression_** or **_declaration_**, where an **_expression_** is a well-structured combination of constants, variables, and functions.


- **Expressions** may be values, combinations of values, and/or functions applied to values. Expressions evaluate to a result. 


- In the case of a **literal value**, the evaluation is trivial, as **it only evaluates to itself**. In the case of an arithmetic equation, the evaluation process is the process of computing the operator and its arguments, as you might expect.  
  But, even though **not** all of your programs will be about doing arithmetic, **all of Haskell’s expressions work in a similar way, evaluating to a result in a predictable, transparent manner.**

  
- **Expressions** are the building blocks of our programs, and programs themselves are one big expression made up of smaller expressions.


- We say that expressions are in _normal form_ when there are no more evaluation steps that can be taken, or, put differently, when they’ve reached an _irreducible form_.


- **Reducible expressions** are also called _redexes_.


- **Irreducible expression is what we call value**. 

  - In that respect, as in **Lambda-Calculus**, function (_introduced further below_) are value, even tho they can be further applied to produce a new value.
  
  - NO, Literal or Constant and what not which are typically though as the only value such as in language like **Java**, are not the only type of value. This is in line with lambda calculus.


- **Declarations** are toplevel bindings that allow us to **name** expressions.


- The terms **argument** and **parameter** are often used interchangeably. However, it is worthwhile to understand the distinction. 

  - A **parameter**, or **formal parameter**, represents a value that will be passed to a function when that function is called. Thus, **parameters** are usually variables.
  
  - An **argument** is an input value the function is **applied to**.
  
  - **A function’s parameter is bound to an argument when the function is applied to that argument.**




## Functions

- **Expressions** are the most basic units of a Haskell program, and a **function is a specific type of expression**.

- **A function** is an _expression_ that when applied to _an argument_ always returns a result.

- **Functions** start with the name of the function. This is followed by the formal parameters of the function, separated only by white space. Next there is an equals sign, which expresses equality of the terms. Finally, there is an expression that is the body of the function and that can be evaluated to return a value.

    ```haskell
    triple x = x * 3
    ```

    - `triple `is the name of the function we are defining; it is a **function declaration**. Note that it begins with a lowercase letter. 
  
    - `x` is the parameter of the function. **The parameters of our function correspond to the head of a lambda and bind variables that appear in the body expression**. 
  
    - The `=` is used to define **(or declare)** values and functions, it binds values (_note that functions are values_). This is not how we test for equality between two values in Haskell. 
  
    - Following the `=` is the **body of the function**, **an expression that could be evaluated if the function is** _applied_ **to a value**. If `triple` is **_applied_**, the argument it’s applied to will be the value to which the `x` is **bound**.
  
    - **Function names** start with lowercase letters.
  
    - **Variables** must also begin with lowercase letters. They need not be single letters.

    
- Because _they are built purely of expressions_, they will always evaluate to the _same result_ when given the _same values_.


- As in the _lambda calculus_, all _functions_ in Haskell **_take one argument and return one result_**. 


- In Haskell, when it seems like we are passing multiple arguments to a function, we are actually applying _a series of nested functions_, each to one argument. This is called **_currying_**.


- **Functions** are how we factor out a pattern into something we can reuse with different inputs.


- **Functions** can also appear in the _expressions that form the bodies of other functions_ or be used as _arguments to functions_, just as any other value can be.


- **Value** are just **irreducible expression**.


## Evaluation


- **Evaluating an expression**, is reducing the _terms_ until the expression reaches its _simplest form_. 


- Once a _term_ has reached its _simplest form_, we say that it is irreducible or _finished evaluating_. 


- An _expression_ in its _simplest form_, is what we usually called a **_value_**.


- Haskell uses a **_non-strict evaluation_** (sometimes called “_**lazy evaluation**_”) **_strategy_** that _defers_ evaluation of terms until they’re forced to evaluate by other terms that refer to them.


- **Values** are _irreducible_, but **applications of functions to arguments** are _reducible_.


- As in the _lambda calculus_, _application is evaluation_: applying a function to an argument allows evaluation or reduction.


- **Values** are expressions, but they cannot be reduced further. Values, in other words, are a _terminal point of reduction_.

- **Haskell doesn’t evaluate everything to canonical or normal form by default**. Instead, it only evaluates to **weak head normal form (WHNF)** by default. What this means is that not everything will get reduced to its irreducible form immediately. The concept is approximate for illustration purpose in what follow:

    ```haskell
    -- The application of anonymous function to the value 2
    (\f -> (1, 2 + f)) 2 
    -- Reduces to the following in WHNF: 
    (1, 2 + 2)
    -- 2 + 2 is not evaluated to 4 until the last possible moment.
    ```




## Let and Where

- The difference is that **let** introduces an _expression_, so it can be used wherever you can have an _expression_, but **where** is a _declaration_ and _is bound to a surrounding syntactic construct_.

## Hands On

- Head to [Learn.hs](src/Learn.hs) and [LetAndWhere.hs](src/LetAndWhere.hs) for the hands on part of this chapter, which in explore depth the various construct the haskell syntax, and provide solution to the chapter exercises.






 
