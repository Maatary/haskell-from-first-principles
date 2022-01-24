# All You Need Is Lambda



## Equivalences:

Find the equivalent lambda term.



1. λxy.xz

    a. λxz.xz - Because the two variables are bound

    b. **λmn.mz** - One unbound variable, one bound variable and one free variable.

    c. λz.(λx.xz) - Just not the same thing.


2. λxy.xxy 

    a. λmn.mnp - There should not be any free variable.

    b. λx.(λy.xy) - Missing one bound variable x.

    c. **λa.(λb.aab)** - Same Thing.


3. λxyz.zx

    a. λx.(λy.(λz.z)) - Missing x

    b. **λtos.st** - Same thing

    c. λmnp.mn - Wring head variable missing.


## Combinators: 

Determine whether each of the following functions are combinators or not.

1. λx.xxx

    **Yes.** The body only contains x which are also in the head.


2. λxy.zx

    **No.** z is a free variable.


3. λxyz.xy(zx)
    
    **Yes.** All variables in the body are also in the head.


4. λxyz.xy(zxy) 

   **Yes.** All variables in the body are also in the head.


5. λxy.xy(zxy)

    **No.** z is a free variable.

## Normal Form or diverge

Determine whether each of the following expressions can be reduced to a normal form or if they diverge

1. 𝜆𝑥.𝑥𝑥𝑥 

2. (𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦) 

3. (𝜆𝑥.𝑥𝑥𝑥)𝑧



## Beta reduce

Evaluate (that is, beta reduce) each of the following expressions to normal form.

1. (𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤) 

2. (𝜆𝑥.𝜆𝑦.𝑥𝑦𝑦)(𝜆𝑎.𝑎)𝑏 

3. (𝜆𝑦.𝑦)(𝜆𝑥.𝑥𝑥)(𝜆𝑧.𝑧𝑞)



      