# Fundamentals




## Type Signature of Numeric Values


 - When we query the types of numeric values, we see type class information instead of a concrete type, because the compiler doesn’t know which specific numeric type a value is until the type is either declared or the compiler is forced to infer a specific type based on the function (i.e. context). **Indeed, as mentioned in the previous chapter notes, numeric values literals are polymorphic.**


 - For example, `13` may look like an integer to us, but that would only allow us to use it in computations that take integers (and not, say, in fractional division). For that reason, **the compiler gives it the type with the broadest applicability** (_**most polymorphic**_) and **says it’s a constrained polymorphic value with the** `type Num a => a`:


 - See Previous chapter (Basic DataTypes) notes, for more detail on numeric values [basic-datatypes](basic-datatypes/fundamentals.md#numeric-datatypes)