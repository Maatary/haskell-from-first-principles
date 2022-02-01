# Fundamentals


## Reflections:

- Integral and Fractional are type classes. They represent operation on respectively all Integral datatype (Int, Integer, ...) and Fractional datatype(Float, Double, ....). Integral and Fractional are not datatypes.


- In `(/) :: Fractional a => a -> a -> a` , `Fractional a =>` can be red as _with constraint Fractional a we have_  or  _with Instance Fractional a we have_ , or more verbose, _with the constraint of having a Franctional instance for type a_

