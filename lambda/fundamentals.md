# Fundamentals


## Functional programing

The essence of functional programming is that programs are **a combination of expressions**. **Expressions include concrete values, variables, and also functions**. **Functions** have a more specific definition: **they are expressions that are applied to an argument or input and, once applied, can be _reduced_ or _evaluated_**.

Functional programming languages are all based on the **lambda calculus**. Some languages in this general category incorporate features that aren’t translatable into lambda expressions. Haskell is a pure functional language, because it does not.



The word **purity** in functional programming is sometimes also used to mean what is more properly called **referential transparency**.     
**Referential transparency** means that the same function, given the same values to evaluate, will always return the same result in pure functional programming, as they do in math  **_(This is a simplification)_**.


## Lambda Calculus

The lambda calculus has three basic components, or _lambda terms_: expressions, variables, and abstractions. 

- The word expression refers to a superset of all those things: an expression can be a variable name, an abstraction, or a combination of those things. 


- The simplest expression is a single variable. Variables here have no meaning or value; they are only names for potential inputs to functions. 


- An abstraction is a function. It is a lambda term that has a head (a lambda) and a body and is applied to an argument. 


- An argument is an input value. 


- Abstractions consist of two parts: the head and the body. The head of the function is a 𝜆 (lambda) followed by a variable name. The body of the function is another expression.

Example of function

`λx.x`

- The variable named in the head is the parameter and binds all instances of that same variable in the body of the function. That means, when we apply this function to an argument, each `x` in the body of the function will have the value of that argument.


- **The act of applying a lambda function to an argument is called application, and application is the lynchpin of the lambda calculus.**


- The lambda abstraction `λx.x` has no name. It is an anonymous function. _A named function can be called by name by another function; an anonymous function cannot._


- The dot (.) separates the parameters of the lambda from the function body. 


- The abstraction as a whole has no name, but the reason we call it an abstraction is that it is a generalization, or abstraction, from a concrete instance of a problem, and it abstracts through the introduction of names. 


- The names stand for particular values, but by using named variables, we allow for the possibility of applying the general function to different values (or, perhaps even values of different types). 


- When we apply the abstraction to arguments, we replace the names with values, making it concrete.


## Beta Reduction

- When we apply a function to an argument, we substitute the input expression for all instances of bound variables within the body of the abstraction. You also eliminate the head of the abstraction, since its only purpose is to bind a variable. This process is called _beta reduction_.

Example of application

`(λx.x) 2`

`2`

- Beta reduction is this process of applying a lambda term to an argument, replacing the bound variables with the value of the argument, and eliminating the head.

- Applications in the lambda calculus are left associative.

`(λx.x)(λy.y)z`

is equivalent to

`((λx.x)(λy.y))z`

which reduce to

`z`

- Beta reduction stops when there are no longer unevaluated functions applied to arguments.

- Sometimes, the body expression has variables that are not named in the head. We call those variables free variables.

`λx.xy`


## Multiple arguments


- Each lambda can only bind one parameter and can only accept one argument. **Functions that require multiple arguments have multiple, nested heads.** **When you apply it once and eliminate the first (leftmost) head, the next one is applied and so on.** This **formulation** was originally discovered by Moses Schönfinkel in the 1920s but was later rediscovered and named after **_Haskell Curry_** and is commonly called **_currying_**.


`λxy.xy`

is a convienient shorthand for

`λx.(λy.xy)`

- When applying the first argument, we’re binding `x`, eliminating the outer lambda, and **get `λy.xy` with `x` being whatever the outer lambda was bound to**.


## Evaluation is simplification

- There are multiple normal forms in lambda calculus, here we will focus and mean **beta normal form**. 


- **Beta normal form** is when you cannot beta reduce (apply lambdas to arguments) the terms any further. This corresponds to a fully evaluated expression, or, **in programming**, **a fully executed program**.


    The expression 2000 / 1000 is not fully evaluated. The division function has been fully applied to two arguments, but it hasn’t yet been reduced or evaluated. In other words, there’s a simpler form it can be reduced to—the number two. The normal form, therefore, is 2.

- Application is what makes evaluation/simplification possible.

- Normal form means there is nothing left that can be reduced.

## Combinators

- A combinator is a lambda term with no free variables. Combinators, as the name suggests, serve only to combine the arguments they are given.


- We won’t have a lot to say about combinators, per se. The point is to call out a special class of lambda expressions that can only combine the arguments they are given, without introducing any new values or random data.



## Divergence


- Not all reducible lambda terms reduce to a normal form. This isn’t because they’re already fully reduced, but because they diverge. Divergence here means that the reduction process never terminates or ends.


Example of divergent lambda term called **omega**

`(λx.xx)(λx.xx)`

Applying the first lambda to the argument give us back the same expression.

 

 

