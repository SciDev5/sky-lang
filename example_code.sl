def (vi, t, g): real

let y = 1/2*g*t^2 + vi*t
let v = diff(t) { y }  // ->  g*t

let solutions = solve(x) {
    require v = 0   // shorthand for  `require(v, 0)` or `require_zero(v)`
} // -> [ vi / g ]

let solutions_subbed = substitute(solutions) {
    set vi = 3  // shorthand for `set(vi, 3)
    set g = 9.81
}   // -> [ 100 / 327 ]
print(decimal(solutions_subbed[0], sig_figs=2))   // -> 0.31


////////////////////////////////////////////////////////////////////////// matrices and tensors


def x: real   // shorthand for `let x: $real = symbol<x>`

// define tensors as `#<rank>[ [ ... ], [ ... ], ... ]`
let arr: tensor<int,[3]> = #<1>[1,2,3]
let arr2: tensor<int,[3,2,2]> = #<3>[[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]]]

let arr2_sum = #sum (i,j,k) { arr2[i,j,k] }
let arr2_sum_short = #sum arr2

let xx: mat<$real,2,2> = #[ // matrix
    1, 1;
    x, 2;
]
let vec: mat<int,1,2> = #[3;4] // column vector
let vec_times_arr: mat<$real,1,2> = xx * vec  // is also a `tensor<$real,[1,2]>`
let vec_inner = vec' * vec
let vec_outer = vec * vec'

let vec_inner_explicit: mat = #sum (i) { vec[i] * vec[i] } // `i: index<2>`, implied by usage in `vec[i]`
let vec_outer_explicit: mat<int,2,2> = #_ [i,j] { vec[i] * vec[j] }
let matmul_explicit: mat<$real,1,2> = #sum [i] (j) { xx[j,i] * vec[j] }
let vec_tensor_prod_explicit = #_ [k(i,j)] { vec[i] * vec[j] }  // k implicitly equals i + dim<i>*j


solve(x) {
    require det(xx) = 0   // expands to `1*2-1*x=0`
} // -> [ 2 ]

///////////////////////////////////////////////////////////////////////////// lists

// one dimensional lists of varying length
let mut list: list<int> = [4,5,6]
list.push(7)
list.push_start(3)
println(list) // [3,4,5,6,7]

list.length() // 5

list.pop() // 7
list.pop_start() // 3

list.length() // 3

////////////////////////////////////////////////////////////////////////////// plotting

def x: real;

let data_x = [0.0,4.2,2.3,7.4]    // data_x: list<f64>
let data_y = [1.2,1.2,8.3,-3.4]
let data_z = [2.2,2.2,1.0,1.0]

plot {  // draws on a single plot and shows it
    eq(x, x^2) {
        legend("y = x^2")
        line_color(RED)
    }
    line(data_x, data_y)
    
    plot_legend(["data_y"])
    xlabel("the title")
    xlabel("the x axis")
    ylabel("the y axis")
}

plot_grid(2,1) {  // multiple plots
    subplot(0,0) {  // subplots can have multiple lines/graphs
        line(data_x, data_y) {
            with_error_bars(data_z)
        }
        histogram(data_z)
    }
    subplot(1,0) {
        scatter(data_x, data_y) {
            marker_color(BLUE)
        }
    }
}

////////////////////////////////////////////////////////////////////////////// conditionals and looping

if x > 3 {
    // ...
} elif x > 0 {
    // ...
} else {
    // ...
}

for value in [1,2,4] { // only allowed for rank 1 tensors
    println(value) // 1, 2, 4
}
for val_with_index in enumerate([1,2,3]) {
    println(val_with_index) // (0, 1), (1, 2), (2, 4)
}
for val_with_index in enumerate(#<2>[[10,5],[2,1]]) { // enumerate turns it into a 1d iter.
    println(val_with_index) // ((0,0), 10), ((1,0), 5), ((0,1), 2), ((1,1), 1)
}

// loop until `break`
let ret = loop {    
    let condition = // ...
    let val = // ...

    if condition {
        break val
    }
}
// ret <- val

////////////////////////////////////////////////////////////////////////////// functions, exceptions, nulls

fn run() {
    print("hello world")
}

// typescript style type annotations, but with #N as a shorthand for `N: index`
fn calculate<#N, T: linear>(a: mat<T, #N, #N>, b: mat<T, #N, #N>): mat<T, #N, #N> {
    a * b + b * a
}

fn weak_or_inferred_types(a, b) {
    a - b
}

fn explicit_return() {
    return 2
}

fn something_that_could_break(value: T, x: number): f64 !<string> {  //  expands to `: fallible<f64, string>`
    if x > 0 {
        throw "literally any data here"
    } else {
        return 0.0
    }
}

fn fallible_thing_but_inferred_failure<T>(v: T): T ! {
    // ...
    // error type is inferred/unchecked
}

fn something_that_could_return_null<T>(value: T, x: number): T? {
    if x > 0 {
        value 
    } else {
        null
    }
}


fn nullable_shorthands(): int? ! {
    let a: int = something_nullable() ?: return null  // elvis operator, executes line if null
    let b: int = something_nullable()?  // short circuit returns null
    let c: int = something_fallible()!  // short circuit throws
    let (d: int?, err: string?) = something_fallible() // destructuring to nullables, maybe, seems unsavory
    let e: int = catch_recover(something_fallible()) { err ->    // catch_recover<T,E>(result: fallible<T,E>): T
        print("oh no! anyway")
        0
    }
    let f: int = catch_nullify(something_fallible()) { err ->    // catch_nullify<T,E>(result: fallible<T,E>): T?
        print("oh no! short circuit to null")
    }? // short circuits
}

fn null_stacking<T>(): T? {
    let a = null // fails, not enough information to infer nullified type
    let b: T?? = null // null, two layers deep (bear with me)
    let c: T? = b? // ignore the fact that this will always return
    // notice how c is still nullable
    // (note: nulls in autoreturns get casted to the return type's outermost null type)
    let d: T = c?
    // finally a non null type

    // "WHY?? wtff" you may ask. The answer is above, in nullable_shorthands, when you want to use 
    // something like `catch_nullify` on a nullable data type, the stacking lets you peel back precisely
    // one layer of nullability, leaving the inner one intact.

    let e: T?? = null // explicit nulls are at the outermost level (eg. will short circuit if `?` is used)
    let f: T? = e? // short circuits
    let g: T?? = some(null) // innermost null, (one level down), shorts on the third short circuit
    let h: T? = g? // does not short circuit
    let i: T = h? // short circuits
}

// TODO:
// - where does `some` come from?
// - should I capitalize `nullable` and `fallible`?
// - should I add a rust-like enum system to accomidate them?
// - 

//////////////////////////////////////////////////////////////////////////////////// closures


// define a functions that takes a callback, `x` is the default parameter name for the callback
fn function_with_callback(callback: (x: f64) -> f64): void {
    let out: f64 = callback(12.34)
    println(out)
}

//// there are a few ways to call this:

// pass the closure function as a parameter
function_with_callback({ x^2 })

// or preferably, move the closure out past the parentheses
// (NOTE: only allowed if the closure is the last parameter in the function)
function_with_callback() { x^2 }

// this is also valid, and less verbose
function_with_callback { x^2 }

// additionally you can give the closure parameters explicit names
function_with_callback { y -> y^2 }


// you can also add closures as return types for functions
fn returning_closure(): () -> void {
    // ...
}

// closures as return types of closures need clarifying parentheses
let c: () -> (() -> void) = {
    println("what")
    return { println("how") } // clarifying return is mandatory here, otherwise it would be interpreted as a parameter of println
}

// you can also put more complex typings on closures
// `fn` is only required for the template types
// (NOTE: parentheses around the whole thing are recommended but not required)
let d: (fn<T,R>(k: T??, r_gen: () -> R) -> T? !<R>) = // ...


//////////////////////////////////////////////////////////////////////////////////// type aliasing


type TheType = tensor<(()-> void !<string>), [4,4,4]>