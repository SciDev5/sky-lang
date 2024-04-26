import bind_arduino.[_f2i, _mul_f]

pub fn begin() {
    $include ["PRIZM.h"]
    $emit_at_top "PRIZM prizm;"
    $emit "prizm.PrizmBegin()"
}

pub fn set_motor_speeds(rad_per_sec_0: float, rad_per_sec_1: float) {
    let rad_2_deg = 57.29577951308232;
    let speed_0 = _f2i(_mul_f(rad_per_sec_0, rad_2_deg)) 
    let speed_1 = _f2i(_mul_f(rad_per_sec_1, rad_2_deg))
    $emit "prizm.setMotorSpeeds(`speed_0`,`speed_1`)"
}