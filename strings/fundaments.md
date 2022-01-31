# Fundamentals

## Types / DataTypes

 - **Types** are a way of categorizing values.

 - A **type** or **datatype** is a _classification_ of values or data. **Types** in Haskell determine what values are members of the type or that inhabit the type. **_Unlike in other languages, datatypes in Haskell by default do not delimit the operations that can be performed on that data._**

- The `::` symbol is read as “has the type.” It is used to define the type signature of named expressions (values, function or any combination of them) i.e. a line of code that defines the types for a value, expression, or function.

 -  In Haskell, as per the Prelude, **String** is represented by a _linked list_ of Char values, aka `[Char]`. String is effectively a _type alias_ for `[Char]`. Everything that works on list works on String.


- **Type classes** provide definitions of operations, or functions, that can be shared across sets of types.


## Bindings

- **Local bindings** are bindings local to particular expressions. The primary distinction here from top level bindings is that local bindings cannot be imported by other programs or modules.


- **Top level bindings** in Haskell are bindings that stand outside any other declaration. The main feature of top-level bindings is that they can be made available to other modules within your programs or to other people’s programs.


- When the compiler reads a module, it will see all the top-level declarations, no matter in what order they come.


- The **where** and **let** _clauses_ in Haskell introduce _local bindings_ or _declarations_. To _bind_ or _declare_ something means _to give an expression a name_.

## Main and Side-Effects

- **main** is the default action when we _build an executable_ or _run it_ in a REPL. **It is not a function** but is often **_a series of instructions to execute, which can include applying functions and producing side effects_**.


- **main** has the type **IO ()**. **IO**, or **I/O**, stands for _input/output_. In Haskell, it is a special type, called **IO**, used when the result of running a program involves effects beyond evaluating a function or expression.


- _Printing to the screen is an effect_, so printing the output of a module must be wrapped in this IO type. When you enter functions directly into the REPL, GHCi implicitly understands and implements IO without you having to specify that.


- **The do syntax** is a special syntax that allows for _sequencing actions_. It is most commonly used to sequence the actions that constitute your program, some of which will necessarily perform effects such as printing to the screen (that’s why the obligatory type of main is IO ()). _do notation isn’t strictly necessary_, but since it often makes for more readable code than the alternatives,






