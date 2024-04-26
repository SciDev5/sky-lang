pub fn serial_begin(baud: int) {
    $emit_stmt "Serial.begin(`baud`)"
}
pub fn println(text: string) {
    $emit_stmt "Serial.println(` text `)"
}

pub fn _i2f(v: int) -> float = $emit "((float)` v `)"
pub fn _f2i(v: float) -> int = $emit "((int)` v `)"

pub fn _add_i(a: int, b: int) -> int = $emit "(` a `+` b `)"
pub fn _sub_i(a: int, b: int) -> int = $emit "(` a `-` b `)"
pub fn _mul_i(a: int, b: int) -> int = $emit "(` a `*` b `)"
pub fn _div_i(a: int, b: int) -> int = $emit "(` a `/` b `)"

pub fn _add_f(a: float, b: float) -> float = $emit "(` a `+` b `)"
pub fn _sub_f(a: float, b: float) -> float = $emit "(` a `-` b `)"
pub fn _mul_f(a: float, b: float) -> float = $emit "(` a `*` b `)"
pub fn _div_f(a: float, b: float) -> float = $emit "(` a `/` b `)"