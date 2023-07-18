# Solid Command Sequence
a prototype script language.
idea: simple and explicit.
idea2: simplified functional programming.
the language does not have to be turing-complete, and may not use control structures if it does not need them.

## assignment and casting
Every assignment accepts both a specialisation of the type and a generalisation of the type.
You can implicitly cast up or down, but not both in one statement.
Casting down (to a specialisation) results in an optional of the specialisation

## variables
A value is immutable, and _always_ effectively copied.
a closing bracket ends a function. If the last statement has no assignment, then the result of that statement is the return of the function

## Types
All types are structs, like in Rust.
Structs may extend other structs, inheriting all functions of that struct.

## Buffers
Buffers (sometimes called arrays) are abstract collections of some type `T`.
Any member-function of `T` can be applied to `T[]`. This applies the function to each element concurrently, collecting the results as a buffer.

## Functions
An `extern` function describes a c (cdecl) function.
When extending a struct, member functions with return type `This` will gain the return type of the extending struct.
A function may be overloaded with different parameters, but also different return values.
`extern` functions, however may not be overloaded, and must have a unique name.

## Function objects (lamdas)
A `fn<(A a)R>` is a function that accepts argument `a` of type `A` and return value `R`. 
A {...} block always evaluates to a `fn`, 
A (A a){...} block evaluates to a `fn<(A a)R>` for some implied R, possibly void.
The type `fn<T>` is equal to `fn<()T>`.
The type `fn` is equal to `fn<()void>`.
A {...} block may implicitly capture any variable in its scope
Captures are _always_ effectively copied, making `fn` immutable.
Whenever a `fn<T>` is assigned to a variable of `T`, it is evaluated. A `fn<T>` is never evaluated twice.
A `fn<T>` has no member functions, but you can call any member of `T`, causing the `fn` to be evaluated.
A variable of type `T` may also be assigned to a variable of `fn<T>`, resulting in a function that just returns the value. This allows for lazy arguments without much syntactic overhead.
Nothing extends `fn<(A)R>` for any `A` and `R`,

