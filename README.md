# sky-lang

> WORK IN PROGRESS: this codebase is not currently complete and is
> far from clean. I promise to finish it Soon™.
>
> currently working on: pointers and static type analysis

Sky-lang is an experimental programming language aimed at writing code
that can be compiled to targets with all kinds of memory management
paradigms (currently aiming to target systems with managed memory
(native and similar), automatic memory (jvm / other languages),
and global-only memory (microcontrollers)).

Brief example code:

```
// ---- Internal definition of trait Add --- //

@bind_op_trait Add
trait Add<Rhs> {
    type Output
    fn add(lhs: Self, rhs: Rhs): Self.Output
}


// ---- empty datatype ---- //

data TheUnitTypeNameIdk unit


// ---- struct datatype ---- //

data AmazingStruct {
  x: TheUnitTypeNameIdk
  y: i32
} impl {
    /// some words describing the function
    /// (implicitly returns i32)
    fn so_cool_lol(self, z: i32) =
        self.y + z
} impl Add<AmazingStruct> {
    fn add(lhs, rhs) =
        AmazingStruct.{
            x = TheUnitTypeNameIdk,
            y = lhs.y + y,
        }
}


// ---- functions! ---- //

fn implicit_void() {
    do_something()
    do_something_else()
    // returns void
}
fn implicit_return_inline() = 
    2 + 2 // returns 4

fn implicit_return_block() = {
    do_something()
    2 + 2
} // returns 4


// ... and more ... //
```
