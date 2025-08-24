# Faux scripting  language
A prototype scripting language.
The aim for this language is to be concise, with a focus on explicit movement of data, high type safety, and intended to be used in scripts.
It is a static-typed, functional-like language based on method chaining, with pure functions and a lack of if and for statements.
It provides operations on collections using 'streams' which are effectively implemented as streams, with should be versatile enough to replace for-loops.
It provides minimal syntax for currying, creating lambda functions, monadic operations, and error handling.
It is not suited for implementing algorithms.

## Variables
A variable is immutable, and _always_ effectively copied.
Memory is managed by the language

## Types and casting
There are 4 types.
Derived types are types that are a specialisation of another type.
Enums define a set of unique values, similar to a C-enum.
Variants define a set of unique values with data contained in them, similar to Rust-enums.
External types are types without a definition, and refer to types defined in a different language.
External types can be passed around, but not be read from or written to.

Every value can be assigned to both a generalisation of its type or a specialisation of its type.
Casting down (to a specialisation) must happen explicitly, and results in an optional of the specialisation.

## Results, Optionals and booleans
Results are a built-in variant of one positive value and one negative value.
A results of types `P` and `N` is written as `P!N`, and implements a number of built-in functions
Optionals are results where the negative value is of type `void`. Put differently, it defines that some value may not exist.
An optional of type `T` is written as `T?` and implements some additional functions over results.
`boolean`s are of type `void!void`.
The implication of this is that the result of a comparison can be mapped to a Result in a natural way.
On the other side, a Result and a Optional cannot be cast to a `boolean`, but conversion functions exist (`is_some` and `is_pos`).
The two literals `true` and `false` map to a positive and negative `boolean` respectively.

## Statements
Statements are ordered such that functions are generally executed in the order of appearance.
This also means that every statement ends with a _post-fix_ assignment, and that operator precedence _does not exist_.
Arithmetic on numbers must be executed by means of explicitly calling functions.
Again, this language is not meant for algorithms.

Assignment can also be conditionally executed, by passing an assignment as a lambda (more about that later)
Assigning to `return` implies returning from the function, conditionally assigning `return` is a conditional return from the function.
Conditionally assigned variables are implicit monadic types: if the variable is not assigned at runtime, then any function call on the variable is not executed.

## Operators and Symbols
Faux heavily relies on symbols for its conciseness.
Many common operations have a shorthand symbol associated with them.
The Lexer implementation defines which symbols are not eligible for use as operators.
As of writing, this is: `'(', ')', '[', ']', '{', '}', ';', '.', '=', '/', '"', '_'`

## Streams
Streams are abstract collections of some type `T`.
Streams implement the `map` function to transform all element in the stream to new elements.
Streams implement the `filter` function that maps all elements to a Result of the original element; this does not change the stream's size.
Streams implement the `reduce` function to combine all elements.
Streams, and all functions on them, shall be implemented in a way that the full collection never has to materialize.
As a result, infinite streams are possible.

## Functions
When extending a struct, member functions with return type `This` will gain the return type of the extending struct.
A function may be overloaded with different parameters, but not different return values.
A closing bracket ends a function. If the last statement does not end in an assignment, then the result of that statement is the return of the function.
An `extern` function describes a c `cdecl` function.
`extern` functions may not be overloaded, and must have a unique name.

## Function objects (lambdas)
A `fn<(A a)R>` is a lambda function that accepts one argument `a` of type `A` and return value `R`. 
Multiple parameters are allowed: `fn<(A, B, C)R>` is valid for some types `A`, `B`, `C` and `R`.
The argument names are optional in the type definition.
A `(A a):R{...}` block evaluates to a `fn<(A)R>`.
A `(A a){...}` block evaluates to a `fn<(A)R>` for some implied `R`.
A `{...}` block evaluates to a `fn<(A)R>` for some implied `A` and `R`, if the first expression of the block evaluates to some function accepting `A`
The type `fn<T>` is equal to `fn<()T>`.
A lambda may implicitly capture any variable in its scope
Captures are _always_ effectively copied, and `fn` is state-less and immutable.
Whenever a `fn<T>` is assigned to a variable of `T`, it is evaluated.
A `fn<T>` has no member functions, but you can call any member of `T`, causing the `fn` to be evaluated.
A variable of type `T` may also be assigned to a variable of `fn<T>`, resulting in a function that just returns the value. 
This allows for lazy arguments without much syntactic overhead.
Nothing extends `fn<(A)R>` for any `A` and `R`,

## The no-return type or BANG type
The `!` type is used to indicate the end of control flow.
Only the assignment expression returns the `!` type, but lambdas of type `fn<(T)!>` can be made from assignments for any `T`.
These lambdas cannot be assigned to variables, but can be passed to functions.
Executing such a lambda forces the function to return. 
Notice that executing an assignment normally only forces a return when the assignment assigns to `return`.
