> # > SKY-LANG

# Build / Toolchain

## Package management

### Package Naming Scheme:
>`[ @organization/ ] package_name [ /child_package ] [ :version ] [ @subpackage ]`

>`git( package_git_url ) [ /child_package ] [ :version ] [ @subpackage ]`

Ex:
- `@cats_incorperated/meow`
- `http_client:2.23.3_alpha`
- `virtual_dom@mock`
- `num/complex`
- `@someone_probably/gigantic_package/with_a_million_children:43.22.2453_rc3@building`
- `is_even`
- `git(https://example.com/whatever.git)`


Notice: the package name `skynet` is reserved to be a machine learning library.

### Child packages

One master package can contain several children.

The child packages are stored in `children/<child_package_name>/`.
TODO It is not currently decided if child packages may contain children.

### Subpackages

One package / child packge can have several subpackages that vary by
what they do *in the toolchain*.

Note: All external libraries must be compiled only. When running in
interpreted mode, these will be turned into interpreter bytecode
modules, while in executable compiled mode, they will be turned
to object files.

TODO naming

| folder | visibility (and name) | usage |
|-|-|-|
| `src_exec/` | internal | Executable source. |
| `src/` | external (`src`) | The primary package source. |
| `builder/` | external (`builder`) | Dynamic building code library. |
| `test/blackbox/` | internal | Code that contains full integration tests. |
| `test/mock/` | external (`mock`) | A full mock of the package that can be used in unit tests (requires ). |

## Dynamic building

TODO Something about being able to reconfigure the compiler to neatly
fit starkly different use cases, such as executable compilation with
LLVM, compiling to WebAssembley and generating glue code, programming microcontrollers, or anything.

This part allows utilizing skycode to transform itself (these are `builder` subpackages).

TODO how to handle arbitrary build targets

# Name ideas
- Sky (sky-lang)
> "sky's the limit"

| ext | usage |
|-|-|
| `.si` | Source file (interpreted) |
| `.sc` | Source file (compiled) |
| `.sg` | Source file (graphics/compute) |
| `.sm` | Interpreter bytecode module |
| `.sdlm` | Interpreter bytecode module for shimming dynamic linked libraries. |
| `.snb` | Notebook file (interpreted) |

- Xenon
- Iridium
- SkyNB
- SkyLab
- SciCat
- Quartz

# data

## primitive
|type|contents|more info|
|-|-|-|
|`int`| signed 128 bit int
|`float`| 64 bit float
|`rational`| i64 / u64
|`complex`| 2x 64 bit float | complex number
|`bool`| boolean
|`char`| character


## core objects
|type|contents|
|-|-|
|`string`| immutable string |
|`function`| bytecode function |
|`class`| custom class |
|`class_instance`| custom class instance |
|`enum`| custom enum class, rust like |
|`enum_instance`| custom enum class instance |
|`array<T,l>`| fixed-size array of T |

## derived objects
|type|contents|derive|aka|
|-|-|-|-|
|`fallible<T,E>`| success T or error R | `enum` | `T!<E>` |
|`nullable<T>`| value T or null | `enum` | `T?` |
|`tensor<T,[...dim]>`| syx tensor | `class` |
|`matrix<T,w,h>`| rank 2 syx tensor | `class` |
|`list<T>`| growable list of T | `class` |
|`map<K, V>`| map of K to V as rust's HashMap | `class` |
|`set<T>`| set of distinct T as rust's HashSet | `class` |
|`sigfig`| number with significant figures (complex number) | `class` |

## units
Units can be attatched to some numeric types:

### syntax:
- literals: `1.23 \m` or `123 \<apples>` or `1.23 \[m s^-2]`
- types: `let KE: float\J` or `let ppl: int\<people>` or `fn Z(): complex[kg m^2 / s^3 A^2]`

### unit declarations
- `units people`
- `units C6H12O6, H2O`
- `units H+, CO3-2, Ba_2-`, (legal unit name `/^[a-z][a-z0-9+-_]*$/i`)

### unit aliasing
- `units N = [kg m / s]`

### base units
|name|internal|meaning|
|-|-|-|
|`<T>`| `count<T>` | count of something (label is an object) |
|`m`  |`(si<1,0,0,0,0,0,0>, 1)` |
|`cm` |`(si<1,0,0,0,0,0,0>, 1/100)` |
|`s`  |`(si<0,1,0,0,0,0,0>, 1)` |
| ... | ...


## data impl (interpreter)
```
[value]:
    | primitive
    | gc_ref<object_all>

[object_all]:
    | *

[function]:
    - args: Vec<Type>
    - return: Type
    - content:
        | builtin ( Fn(Vec<Value>)->Value )
        | bytecode ( Vec<Instruction> )

[class]:
    - propertyLUT: HashMap<&str, (usize, Type)>
    - opOverloadLUT: HashMap<(SLOperator, binary|unary), (usize, Type)>
    - opOverloadFn: Vec<function(
        a:Self/instance | a:Self/instance,b:Self/instance
    ) -> R>
    - instantiate(Vec<Value>) -> Self/instance
    - static: Option<object>
[class_instance]:
    - class: Self/..
    - property: Vec<Value>

[object]:
    - propertyLUT: HashMap<&str, usize>
    - opOverloadLUT: HashMap<SLOperator, usize>
    - property: Vec<Value>

[enum]:
    - propertyLUT: HashMap<&str, usize>
    - opOverloadLUT: HashMap<SLOperator, usize>
    - 
```

# Annotation processing

Defined in `builder` subpackages. Can transform tokens or AST.

# Errors and linting

## Compilation and linting errors

The parsing and type-generation step may produce errors.

When compiling, any errors present are blocking and will
not allow code execution.

When interpreting, errors will be worked around as well as
possible.
- Localized syntax errors and type mismatches
get replaced with panics that terminate if reached.
- Non-localized syntax errors such as mismatched parentheses
are blocking and prevent execution in its entirety.

## Runtime errors
Errors are recoverable, and propagated up the stack by
returning a `fallible<T,E>`.

Errors should capture where they were created, pointing to
one or two tokens that caused the issue, and should also
have a message explaining why they were created.

### Shorthands:
There is shorthand syntax for `fallible<T,E>`.
```skycode
// The `!` shorthand type makes writing fallible data breifer.
// The following type expands to `fallible<string, SomeError>`:
let string_or_error: string !<SomeError>

// If the `<...>` after the `!` is omitted, the compiler will
// attempt to infer the error type.
fn inferred_error_type(): float ! { /* ... */ }
```

Infix operators will automatically unwrap `fallible`s and
early return with it if there's an error. If panics are allowed
and the function does not return a `fallible` of the right type,
the operation will instead panic, and a type warning will be raised.

```skycode
// automatically early-returns SysError.DivByZero if `b == 0.0`
fn fallible_math(a: float, b: float): ! { // or `float !<SysError>`
    a / b
}
```

There is also a shorthand for short circuit returning `fallible`s
if the're the error variant.
```skycode
// the `?` shorthand shortcuts
fn the_function(a: int): int ! {
    let b: float = fallible_math(1.0, 0.0)?
    return a + round(b)
}
```

## Runtime panics
Panics are guaranteed failure and cannot be recovered from.
They result in an immediate termination of all the process's
activity, all data and resources are released as part of this
process.

Panics are not allowed in library code.

## Changing behavior of builtin failures:

Behavior can be changed with annotations and other macros.
- division by zero is always an error unless annotated as `@allow(float_divide_by_zero)`



# Weird things to think about

- TODO: figure out what to do about floating point equality 
- TODO: figure out what to do about `NaN`s 
- 