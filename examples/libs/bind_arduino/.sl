//-------------- Serial Monitor --------------//

pub fn serial_begin(baud: int) {
    $emit_stmt "Serial.begin(`baud`)"
}
pub fn println(text: string) {
    $emit_stmt "Serial.println(` text `)"
}

//-------------- Math --------------//

pub fn _i2f(v: int) -> float = $emit "(float)` v ` "
pub fn _f2i(v: float) -> int = $emit "(int)` v ` "

pub fn _add_i(a: int, b: int) -> int = $emit " ` a `+` b ` "
pub fn _sub_i(a: int, b: int) -> int = $emit " ` a `-` b ` "
pub fn _mul_i(a: int, b: int) -> int = $emit " ` a `*` b ` "
pub fn _div_i(a: int, b: int) -> int = $emit " ` a `/` b ` "

pub fn _add_f(a: float, b: float) -> float = $emit " ` a `+` b ` "
pub fn _sub_f(a: float, b: float) -> float = $emit " ` a `-` b ` "
pub fn _mul_f(a: float, b: float) -> float = $emit " ` a `*` b ` "
pub fn _div_f(a: float, b: float) -> float = $emit " ` a `/` b ` "

pub fn sin(x: float) -> float = $emit "sin(` x `)"
pub fn cos(x: float) -> float = $emit "cos(` x `)"
pub fn tan(x: float) -> float = $emit "tan(` x `)"
pub fn atan2(y: float, x: float) -> float = $emit "atan2(` y `,` x `)"

pub fn abs(x: float) -> float = $emit "abs(` x `)"
pub fn sqrt(x: float) -> float = $emit "sqrt(` x `)"

// :: Utils :: //
pub fn rad_2_deg(rad: float) -> float = _mul_f(rad, 57.29577951308232)
pub fn deg_2_rad(deg: float) -> float = _div_f(deg, 57.29577951308232)

//-------------- Arduino --------------//

// :: Time :: //
pub fn millis() -> int = $emit "(int) millis()"
pub fn delay(t: int) = $emit "delay((unsigned long)` t `)"
pub fn delay_seconds(t: float) = delay(_f2i(_mul_f(t,1000.0)))
