import bind_arduino.[_f2i, _i2f, _add_f, _sub_f, _mul_f, _div_f, delay_seconds, abs, sqrt, sin, cos, clip]
import prizm


fn cm_to_wheel_rad(speed: float) -> float =
    // wheel radius is 5\[cm/rad]
    // speed_out\[deg/s] = speed\[cm/s] / (5\[cm/rad]) * (180\deg / PI\rad)
    $emit "(`speed` / 5)"

fn wheel_deg_to_cm(speed: int) =
    // wheel radius is 5\[cm/rad]
    // speed_out\[cm/s] = speed\[deg/s] * (5\[cm/rad]) / (180\deg / PI\rad)
    clip(_i2f($emit " `speed` * 5 / (180 / PI)"), -720.0, 720.0)


// Set the speed of Y bot
pub fn ybot_set_vel(x: float, y: float, xy: float) {
    let wx = cm_to_wheel_rad(x)
    let wy = cm_to_wheel_rad(y)

    prizm.set_motor_speeds(
        $emit "(-`wx` + (M0_R * `xy`))",
        $emit "(-`wy` + (M1_R * `xy`))",
    )
    prizm.set_motor_speeds_ext(
        1,
        $emit "(`wx` + `wy`) * sqrt(0.5) + (M2_R * `xy`)",
        0.0,
    )
}
// Get the position and set the speed of Y bot
// and try to move toward a given target.
pub fn ybot_update(tx: float, ty: float, txy: float, t: float) {
    $emit_at_top "static float M0_LAST = 0.0;"
    $emit_at_top "static float M1_LAST = 0.0;"
    $emit_at_top "static float M2_LAST = 0.0;"
    let m0_new = wheel_deg_to_cm(prizm.get_encoder_deg(1))
    let m1_new = wheel_deg_to_cm(prizm.get_encoder_deg(2))
    let m2_new = _mul_f(0.95, wheel_deg_to_cm(prizm.get_encoder_deg_ext(1,1)))
    let m0: float = $emit "(`m0_new` - M0_LAST)"
    let m1: float = $emit "(`m1_new` - M1_LAST)"
    let m2: float = $emit "(`m2_new` - M2_LAST)"
    $emit_stmt "M0_LAST = (`m0_new`)"
    $emit_stmt "M1_LAST = (`m1_new`)"
    $emit_stmt "M2_LAST = (`m2_new`)"

    
    let dxy: float = $emit "(`m0` + `m1` + `m2` * sqrt(2.0)) / (M0_R + M1_R + M2_R * sqrt(2.0))"
    let dx_rel: float = $emit "(`dxy` * M0_R - `m0`)"
    let dy_rel: float = $emit "(`dxy` * M1_R - `m1`)"
    
    $emit_at_top "static double X = 0.0;"
    $emit_at_top "static double Y = 0.0;"
    $emit_at_top "static double XY = 0.0;"

    $emit_stmt "XY += (`dxy`)"
    $emit_stmt "X += `dx_rel`*cos(XY) - `dy_rel`*sin(XY)"
    $emit_stmt "Y += `dx_rel`*sin(XY) + `dy_rel`*cos(XY)"

    $emit_at_top "static double T_LAST = 0.0;"
    let dt = _sub_f(t, $emit "T_LAST")
    $emit_stmt "T_LAST=(`t`)"
    $emit_at_top "static double TX_LAST = 0.0;"
    $emit_at_top "static double TY_LAST = 0.0;"
    $emit_at_top "static double TXY_LAST = 0.0;"
    let tvx = _div_f(_sub_f(tx, $emit "TX_LAST"), dt)
    let tvy = _div_f(_sub_f(ty, $emit "TY_LAST"), dt)
    let tvxy = _div_f(_sub_f(txy, $emit "TXY_LAST"), dt)
    $emit_stmt "TX_LAST=(`tx`)"
    $emit_stmt "TY_LAST=(`ty`)"
    $emit_stmt "TXY_LAST=(`txy`)"

    $emit_stmt "Serial.println(`tvx`)"


    let lin_kcorrect = 1.0 // (cm/s)/cm
    let rot_kcorrect = 0.5 // (rad/s)/rad
    let corrective_vxy = _mul_f(sub_xy(txy, $emit"XY"), rot_kcorrect)
    let corrective_vx = _mul_f(_sub_f(tx, $emit"X"), lin_kcorrect)
    let corrective_vy = _mul_f(_sub_f(ty, $emit"Y"), lin_kcorrect)


    let vx = _add_f(tvx, corrective_vx)
    let vy = _add_f(tvy, corrective_vy)
    let vxy = _add_f(tvxy, corrective_vxy)

    // ybot_set_vel(
    //     vx,
    //     vy,
    //     vxy,
    // )
    ybot_set_vel(
        _add_f(_mul_f(cos($emit"XY"), vx), _mul_f(sin($emit"XY"), vy)),
        _sub_f(_mul_f(cos($emit"XY"), vy), _mul_f(sin($emit"XY"), vx)),
        vxy,
    )
}

fn sub_xy(xya: float, xyb: float) -> float =
    $emit "fmod(fmod(`xya` - `xyb`, 2.0*PI) + 3.0*PI, 2.0*PI) - 1.0*PI"


pub fn ybot_init() {
    $emit_at_top "const double M0_R = 16.0 + 1.6 + 2.0;"  // motor 0 is 19.6 cm away from the center.
    $emit_at_top "const double M1_R = 16.0 + 1.6 + 2.0;"  // motor 1 is 19.6 cm away from the center.
    $emit_at_top "const double M2_R = 9.6 + 1.6 + 2.0;"   // motor 2 is 13.2 cm away from the center.

}
pub fn ybot_stop() {
    prizm.set_motor_speeds(0.0, 0.0)
    prizm.set_motor_speeds_ext(1, 0.0, 0.0)
}