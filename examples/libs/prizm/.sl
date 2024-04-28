import bind_arduino.[_f2i, _mul_f, rad_2_deg]

pub fn begin() {
    $include ["PRIZM.h"]
    $emit_at_top "PRIZM prizm;"
    $emit "prizm.PrizmBegin()"
}
pub fn begin_ext() {
    $emit_at_top "EXPANSION prizm_ext;"
}

pub fn set_motor_speeds(rad_per_sec_0: float, rad_per_sec_1: float) {
    let speed_0 = _f2i(rad_2_deg(rad_per_sec_0))
    let speed_1 = _f2i(rad_2_deg(rad_per_sec_1))
    
    // let rad_2_deg = 57.29577951308232;
    // let speed_0 = _f2i(_mul_f(rad_per_sec_0, rad_2_deg)) 
    // let speed_1 = _f2i(_mul_f(rad_per_sec_1, rad_2_deg))

    $emit "prizm.setMotorSpeeds(`speed_0`,`speed_1`)"
}

 pub fn set_motor_speeds_ext(id: int, rad_per_sec_0: float, rad_per_sec_1: float) {
    let speed_0 = _f2i(rad_2_deg(rad_per_sec_0))
    let speed_1 = _f2i(rad_2_deg(rad_per_sec_1))

    // let rad_2_deg = 57.29577951308232;
    // let speed_0 = _f2i(_mul_f(rad_per_sec_0, rad_2_deg)) 
    // let speed_1 = _f2i(_mul_f(rad_per_sec_1, rad_2_deg))
    
    $emit "prizm_ext.setMotorSpeeds(`id`,`speed_0`,`speed_1`)"
}