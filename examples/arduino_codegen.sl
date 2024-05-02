import bind_arduino.[serial_begin, println, delay_seconds, seconds, sin, cos, _mul_f]
import prizm
import bdbot
import ybot

@name "setup"
fn _setup_() {
    serial_begin(9600)
    prizm.begin()

    ybot.ybot_init()
    //bdbot.do_fun_path()
}

@name "loop"
fn _loop_() {
    println("hello world! :3c")
    let t = seconds()
    $emit_stmt "Serial.println(`t`)"
    //ybot.ybot_set_vel(cos(t),sin(t),0.0)
    let tr = _mul_f(t, 1.0)
    ybot.ybot_update(
        _mul_f(cos(tr),5.0),
        _mul_f(sin(tr),5.0),
        1.5,
        t,
    )
}