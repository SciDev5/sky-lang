import bind_arduino.[serial_begin, println, _add_f, _sub_f, _mul_f, _div_f, delay_seconds, abs]
import prizm

/// Set robot speed in cm/s and rad/s (counter clockwise)
pub fn bdbot_set_speed(fwd: float, turn: float) {
    // wheel diameter: 10.2cm, wheel separation: 25.4cm
    // forward: 5.1cm/rad
    // turn: (12.7cm/rad)/(5.1cm/rad) = 2.49rad/rad
    let fwd = _div_f(fwd, 5.1)
    let turn = _mul_f(turn, 2.49)
    prizm.set_motor_speeds(
        _add_f(turn, fwd),
        _sub_f(turn, fwd),
    )
}

pub fn bdbot_stop() {
    prizm.set_motor_speeds(0.0, 0.0)
}

pub fn bdbot_arc(total_angle: float, r: float, speed: float) {
    let t = _div_f(_mul_f(total_angle, r), speed)
    bdbot_set_speed(speed, _div_f(total_angle,t))
    delay_seconds(abs(t))
}


pub fn do_fun_path() {
    
    let pi = 3.14159
    let two_pi = _mul_f(2.0,pi)
    let half_pi = _mul_f(0.5,pi)

    let out_angle = 1.5
    let turn_radius = 40.0
    let speed = 20.0
    let neg_turn_radius = -40.0

    delay_seconds(0.5)
    bdbot_set_speed(speed, 0.0)
    delay_seconds(1.0)
    bdbot_arc(out_angle, turn_radius, speed)
    bdbot_arc(_mul_f(2.0, out_angle), neg_turn_radius, speed)
    bdbot_arc(out_angle, turn_radius, speed)
    bdbot_set_speed(speed, 0.0)
    delay_seconds(1.0)
    bdbot_stop()
    delay_seconds(0.5)
    
}
