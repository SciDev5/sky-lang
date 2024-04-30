import bind_arduino.[_f2i, _i2f, _add_f, _sub_f, _mul_f, _div_f, delay_seconds, abs, sqrt]
import prizm


fn cm_to_wheel_deg(speed: float) =
    // wheel radius is 5\[cm/rad]
    // speed_out\[deg/s] = speed\[cm/s] / (5\[cm/rad]) * (180\deg / PI\rad)
    _f2i(clip($emit " `speed` / 5 * (180 / PI)", -720.0, 720.0))

fn wheel_deg_to_cm(speed: float) =
    // wheel radius is 5\[cm/rad]
    // speed_out\[cm/s] = speed\[deg/s] * (5\[cm/rad]) / (180\deg / PI\rad)
    clip(_i2f($emit " `speed` * 5 / (180 / PI)"), -720.0, 720.0)


fn coerceGE(x: float, min: float) -> float = $emit "max(`x`, `min`)"
fn coerceLE(x: float, max: float) -> float = $emit "min(`x`, `max`)"
fn clip(x: float, min: float, max: float) = coerceGE(coerceLE(x, max), min)

// Set the speed of Y bot in 
pub fn ybot_set_vel(x: float, y: float, xy: float) {
    let wx = cm_to_wheel_deg(x)
    let wy = cm_to_wheel_deg(y)


    prizm.set_motor_speeds(
        $emit "-`wx` + (19.6 * `xy`)",
        $emit "-`wy` + (19.6 * `xy`)",
    )
    prizm.set_motor_speeds_ext(
        1,
        $emit "(`wx` + `wy`) * sqrt(0.5) + (13.2 * `xy`)",
        0.0,
    )
}



pub fn ybot_stop() {
    prizm.set_motor_speeds(0.0, 0.0)
    prizm.set_motor_speeds_ext(1, 0.0, 0.0)
}