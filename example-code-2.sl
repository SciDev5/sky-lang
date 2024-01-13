struct TheClass<T> {
    data_x: int
    data_t: T
    

    fn associated_fn(self, x: int) {
        self.data_x += x
    }

    fn new(data_t: T) = Self {
        data_x: 2,

    }

    constrain <T: Default> {
        fn new() -> Self {
            // new(T::default())
            new(_)
        }

        autoimpl Default // autoimpls are defined by build submodules

        // impl Add<Self> {
        //     fn add(self, rhs: Self) -> Self {
        //         Self {
        //             data_x: self.data_x + rhs.data_x,
        //             ... // default
        //         }
        //     }
        // }
        
        op (self) + (rhs: Self) = Self { data_x: self.data_x + rhs.data_x, ... }
    }
}

trait Add<T> {
    type Output
    fn add(self, rhs: T) -> Output
}

/// alias `_`
///
/// ```
/// let a: TheClass<BasicData> = _
/// let b: BasicData = _
/// let c = BasicData { ... }
/// ```
trait Default {
    fn default() -> Self
}


struct BasicData {
    pub a: string
    pub b: float
}


enum Option<T> {
    Some(T)
    None

    fn unwrap(self) -> T {
        match self {
            Some(v) => v
            None => panic()
        }
    }

    constrain<T: Copy> {
        autoimpl Copy
    }   
    constrain<T: Clone> {
        autoimpl Clone
    }
}

enum ComplicatedEnum {
    A
    B(BasicData, float)
    C { kfjkjd: int }
}

struct K  {
   tuple (int, int)


}

struct void


defcontext cool_extensions {

    extend<N: Number> Option<T: Plus<N>> {
        fn increment(self) = match self {
            Some(v) => Self(v + 1)
            None => None
        }
    }

    extend Option<=int> {
        fn do_funky_integer_stuff_idk(self) = match self {
            Some(n) => -n + 342
            None
        }
    }


    constrain <T: Copy + Plus<T>> {
        extend T {
            fn add_self(self) = self + self
        }
    }

}


fn func_name(param_a: BasicData) {
    // implicit return type
}
fn func_name(param_a: BasicData) {
    function_that_returns_an_immediately_dropped_value()
    void // return void (still inferred tho) (technically a struct instantiation)
}
fn func_name(param_a: BasicData) -> void {
    // explicit void type
}