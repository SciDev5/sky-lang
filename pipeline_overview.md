# system parts:

- parse ( `str` &rarr; `CommonModule` )
    - lexing/parsing ( `str` &rarr; `Vec<Token>` &rarr; `AST` )
        > transforming text into an Abstract Syntax Tree
    - module_raw ( `AST` &rarr; `RawModule` )
        > isolating static declarations from imperative code 
    - module_common ( `RawModule` &rarr; `CommonModule` )
        > type inference and field/reference resolution
- compile
    - interpreter_bytecode ( `CommonModule` &rarr; `BytecodeModule` )
        > prepare code for use in the interpreter
    - llvm ( `CommonModule` &rarr; TODO )
        > // TODO, probably something about sending stuff to be processed by LLVM once I figure out how LLVM even works.
- interpret ( Executes `BytecodeModule` )
    > should be simple, right?  


## module types

General format:
- functions and classes are all stored in a top-level list and scoping is done referencing into that list.
- functions contain instructions for execution, either as a tree or list of instructions.
- contains a top level function-like object for executing top level instructions and referencing top-level functions and classes


static functions
inline anon_functions
crossinline anon_functions
noinline closure anon_functions
noinline non_closure anon_functions


Specific modules:
- `RawModule`:
    - 1-to-1-ish with text representation
    - class/function definition instructions exist to aid in scoping
    - string identifiers for vars and class/function calls
    - only explicitly-declared typings are known
    - full source map, stored inline
- `CommonModule`:
    - types are fully checked
    - all references are singularly resolved as integer-based references


## type inference

- forward only (eg. `let x: int; let y = x` implies `y: int`, but `let x; x = some_func()` cannot imply a value for x if `some_func`'s return type is not definitely one type only)
- all function parameters must be explicitly typed unless it's a callback passed as a function's parameter or in a spot with explicitly known-ahead-of-time type (eg. `some_func({ a -> /* whatever */ })` is allowed, but `let cb = { a -> /* whatever */ }; some_func(cb)` is not) (functions as return values are allowed to omit types if return type is explicitly known)
- compiled functions have all their types stored.
- the type inference step also handles determining when to allow inlined callbacks, which are stored as an AST node that say inline and contain the callback code.