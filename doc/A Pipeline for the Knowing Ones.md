# Faux 'programming'  language
a prototype script / programming language.
The aim for this language is to function as an easy-to-write programming language, with a focus on explicit movement of data, and high safety.
the language does not have to be turing-complete, and may not use control structures if it does not need them.
It is a static-typed, functional-like language based on currying, with pure functions and a lack of if- and for- statements.
It provides operations on collections using 'buffers' which are effectively implemented as streams, with should be versatile enough to replace for-loops.
It provides minimal syntax for creating lamda functions, monadic operations, and error handling.
The language should not need a garbage collection system, as data is managed in a way similar to Rust's borrow checker.

## Values and casting
Every value can be assigned to both a specialisation of the type and a generalisation of the type.
You can implicitly cast up, and explicitly cast down.
Casting down (to a specialisation) results in an optional of the specialisation.

## Variables
A variable is immutable, and _always_ effectively copied.
The compiler manages the memory implicitly.

## Statements
Every statement ends with a post-fix assignment.
Assignment can also be used as part of a monadic operation, in which case the assignemnt is said to be 'conditional'.
Assigning to `return` implies returning from the function, conditionally assigning `return` is a conditional return from the function.
Conditionally assigned variables are implicit monadic types: if the variable is not assigned at runtime, then any function call on the variable is not executed.

## Types
...

## Buffers
Buffers (sometimes called arrays) are abstract collections of some type `T`.
Buffers implement the `map` function to transform all element in the buffer to new elements.
Buffers implement the `filter` function that maps all elements to a `Result` of the original element; this does not change the buffer's size.
Buffers implement the `reduce` function to combine all elements.
Buffers, and all functions on them, shall be implemented in a way that the full collection never has to materialize.
As a result, infinite buffers are possible.

## Functions
An `extern` function describes a c (cdecl) function.
When extending a struct, member functions with return type `This` will gain the return type of the extending struct.
A function may be overloaded with different parameters, but also different return values.
`extern` functions, however may not be overloaded, and must have a unique name.
a closing bracket ends a function. If the last statement does not end in an assignment, then the result of that statement is the return of the function.

## Function objects (lamdas)
A `fn<(A a)R>` is a lamda function that accepts argument `a` of type `A` and return value `R`. 
The argument name is optional.
A (A a){...} block evaluates to a `fn<(A)R>` for some implied R, possibly void.
The type `fn<T>` is equal to `fn<()T>`.
A lamda may implicitly capture any variable in its scope
Captures are _always_ effectively copied, and `fn` is state-less and immutable.
Whenever a `fn<T>` is assigned to a variable of `T`, it is evaluated. A `fn<T>` is never evaluated twice.
A `fn<T>` has no member functions, but you can call any member of `T`, causing the `fn` to be evaluated.
A variable of type `T` may also be assigned to a variable of `fn<T>`, resulting in a function that just returns the value. This allows for lazy arguments without much syntactic overhead.
Nothing extends `fn<(A)R>` for any `A` and `R`,

