# Faux 'programming'  language
a prototype script / programming language.
The aim for this language is to be concice, with a focus on explicit movement of data, high safety, and intended to be used in combination with C.
It is a static-typed, functional-like language based on method chaining, with pure functions and a lack of if- and for- statements.
It provides operations on collections using 'buffers' which are effectively implemented as streams, with should be versatile enough to replace for-loops.
It provides minimal syntax for currying, creating lamda functions, monadic operations, and error handling.
It is not suited for implementing algorithms.

## Variables
A variable is immutable, and _always_ effectively copied.
The compiler manages the memory implicitly in a way similar to Rust's borrow checker.

## Types and casting
There are 4 types.
Derived types are types that are a specialisation of another type.
Enums define a set of unique values, similar to a C-enum.
Variants define a set of unique values with data contained in them, similar to Rust-enums.
External types are types without a definition, and refer to types defined in a different language.
External types can be passed around, but not be read from or written to.

Every value can be assigned to both a generalisation of its type or a specialisation of its type.
Casting down (to a specialisation) must happen explicitly, and results in an optional of the specialisation.

## Results and booleans
`Result`s are a built-in variant of a generic positive value and a generic negative value.
`Maybe`s are a `Result<void>`.
`boolean`s are a `Result<void, void>` (but not a `Maybe<void>`).
The implication of this is that the result of a comparison can be mapped to a `Result` in a natural way.
On the other side, a `Result` and a `Maybe` cannot be cast to a `boolean`, but conversion functions exist (`is_some` and `is_pos`).

## Statements
Statements are ordered such that functions are generally executed in the order of appearance.
This also means that every statement ends with a _post-fix_ assignment, and that operator precedence _does not exist_.
Arithmetic on numbers must be executed by means of explicitly calling functions.
Again, this language is not meant for algorithms.

Assignment can also be used where a function is expected a monadic operation, in which case the assignment is said to be 'conditional'.
Assigning to `return` implies returning from the function, conditionally assigning `return` is a conditional return from the function.
Conditionally assigned variables are implicit monadic types: if the variable is not assigned at runtime, then any function call on the variable is not executed.

## Operators and Symbols
Faux heavily relies on symbols for its conciceness.
The Lexer implementation defines which symbols are not eligible for use as operators.
As of writing, this is: `'(', ')', '[', ']', '{', '}', ';', '.', '=', '/'`, but also excludes symbols that indicates or kinds of tokens: quotes `"` and underscores `_`

## Buffers
Buffers are abstract collections of some type `T`.
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
A `fn<(A a)R>` is a lamda function that accepts one argument `a` of type `A` and return value `R`. 
Multiple parameters are allowed: `fn<(A, B, C)R>` is valid for some types `A`, `B`, `C` and `R`.
The argument names are optional in the type definition.
A `(A a):R{...}` block evaluates to a `fn<(A)R>`.
A `(A a){...}` block evaluates to a `fn<(A)R>` for some implied `R`.
A `{...}` block evaluates to a `fn<(A)R>` for some implied `A` and `R`, if the first expression of the block evaluates to some function accepting `A`
The type `fn<T>` is equal to `fn<()T>`.
A lamda may implicitly capture any variable in its scope
Captures are _always_ effectively copied, and `fn` is state-less and immutable.
Whenever a `fn<T>` is assigned to a variable of `T`, it is evaluated. A `fn<T>` is never evaluated twice.
A `fn<T>` has no member functions, but you can call any member of `T`, causing the `fn` to be evaluated.
A variable of type `T` may also be assigned to a variable of `fn<T>`, resulting in a function that just returns the value. This allows for lazy arguments without much syntactic overhead.
Nothing extends `fn<(A)R>` for any `A` and `R`,

## The no-return type or BANG type
The `!` type is used to indicate the end of control flow.
Only the assignment expression returns the `!` type, but lamdas can be made from assignments of type `fn<(T)!>` for any `T`.
These lamdas cannot be assigned to variables, but can be passed to functions.
Executing such lamda forces the function to return. 
Notice that executing an assignment normally only forces a return when the assignement assigns to `return`.
