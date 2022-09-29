# Fundamentals

## Types / DataTypes

 - **Types** are a way of categorizing values.

 - A **type** or **datatype** is a _classification_ of values or data. **Types** in Haskell determine what values are members of the type or that inhabit the type. **_Unlike in other languages, datatypes in Haskell by default do not delimit the operations that can be performed on that data._**

- The `::` symbol is read as _**“has the type.”**_ It is used to define **the type signature** of named expressions (values, function or any combination of them) i.e. a line of code that defines the types for a value, expression, or function.


- **Type classes** provide definitions of operations, or functions, that can be shared across sets of types.

## String

- In Haskell, as per the Prelude, **String** is represented by a _linked list_ of _Char values_, aka `[Char]`. **String** is effectively a _type alias_ for `[Char]`. Everything that works on list works on String.


- Concatenation is the joining together of sequences of values. Often in Haskell, this is meant with respect to the `[]`, or list, datatype, which also applies to `String` (which, as we know, is `[Char]`). The concatenation function in Haskell is `++`, which has type `[a] -> [a] -> [a]`.

    ```haskell
    λ> :t (++)
    (++) :: [a] -> [a] -> [a]
  
    λ> "hello" ++ " haskell"
    "hello haskell"
    ```

 - In `(++) :: [a] -> [a] -> [a]` Everything after the `::` is about our types, not our values. The `a` inside the list type constructor [] is a type variable. 

   1. Take an argument of **type** `[a]`. This type is a list of elements of **some type** `a`. **This function does not know what type `a` is**. It doesn’t need to know. In the context of the program, the **type** of `a` will be known and made concrete at some point. 
   
   2. Take another argument of **type** `[a]`, a list of elements whose **type** we don’t know. **Because the variables are the same, they must be the same type throughout** `(a == a)`.
   
   3. Return a result of **type** `[a]`.


- Because **String** is a list containing characters, the operators we use with **strings** can also be used on **lists of other types**, such as lists of numbers. The type `[a]` means that we have a list with elements of a type `a` we do not yet know. If we use the operators to concatenate **lists of numbers**, then the `a` in `[a]` will be **some type of number** (for example, integers). If we are concatenating **lists of characters**, then `a` **represents a Char** because **String** is really type `[Char]`. 


- The type variable `a` in `[a]` is polymorphic. Polymorphism is an important feature of Haskell. We will dwelve into it in the upcoming chapters.


- For **concatenation**, every list must be a list containing the **same type of values**; we cannot concatenate a list of numbers with a list of characters, for example. **However, since the a is a variable at the type level, the literal values in each list we concatenate need not be the same, only the same type**. That is, `(a == a)`



- There is another function concat of which the name can be slightly misleading. The function should actually be called **flatten**, as it is what is does. It **flattens** a **list of list** into a **list**. In that respect it can also be used to concatenate strings, provided that they are placed into a List. 

    ```haskell
    λ> :t concat
    concat :: Foldable t => t [a] -> [a] -- is equivalent to concat :: [[a]] => [a]
    λ> concat  ["hello", " ", "haskell"]
    "hello haskell"
    ```

 - As a side note, it is to be noted that the **square brackets** around **Char** here are the **syntactic sugar** for a list. When we tackle **types** in the  next chapter we will see the full notation for list.



## Bindings

- **Local bindings** are bindings local to particular expressions. The primary distinction here from top level bindings is that local bindings cannot be imported by other programs or modules.


- **Top level bindings** in Haskell are bindings that stand outside any other declaration. The main feature of top-level bindings is that they can be made available to other modules within your programs or to other people’s programs.


- When the compiler reads a module, it will see all the top-level declarations, no matter in what order they come.


- The **where** and **let** _clauses_ in Haskell introduce **_local bindings_** or **_declarations_**. To **bind** or **declare** something means **to give an expression a name**.

    ```haskell
    takeDrop :: Int -> Int -> [a] -> [a]
    takeDrop t d = drop d . take t
  
    rvrs :: [Char]
    rvrs  =
      let
        curryIsAwesome = "Curry is awesome"
        currey = take 5 curryIsAwesome
        is = takeDrop 8 6 curryIsAwesome
        awesome = drop 9 curryIsAwesome
      in concat [awesome, " ",  is, " ", currey ]
    ```

## Main and Side-Effects

- **main** is the default action when we _build an executable_ or _run it_ in a REPL. **It is not a function** but is often **_a series of instructions to execute, which can include applying functions and producing side effects_**.


- **main** has the type **IO ()**. **IO**, or **I/O**, stands for _input/output_. In Haskell, it is a special type, called **IO**, used when the result of running a program involves effects beyond evaluating a function or expression.


- _Printing to the screen is an effect_, so printing the output of a module must be wrapped in this **IO type**. When you enter functions directly into the REPL, GHCi implicitly understands and implements IO without you having to specify that.


- **The do syntax** is a special syntax that allows for _sequencing actions_. It is most commonly used to sequence the actions that constitute your program, some of which will necessarily perform effects such as printing to the screen (that’s why the obligatory type of main is IO ()). _do notation isn’t strictly necessary_, but it often makes for more readable code than the alternatives.

    ```haskell
    main :: IO () 
    main = do 
      putStrLn "Count to four for me:" 
      putStr "one, two" 
      putStr ", three, and" 
      putStrLn " four!"
    ``` 






