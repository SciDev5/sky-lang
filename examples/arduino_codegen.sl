import bind_arduino.[serial_begin, println, delay_seconds, seconds, sin, cos]
import prizm
import bdbot
import ybot

@name "setup"
fn _setup_() {
    serial_begin(9600)
    prizm.begin()

    //bdbot.do_fun_path()
}

@name "loop"
fn _loop_() {
    println("hello world! :3c")
    let t = seconds()
    $emit_stmt "Serial.println(`t`)"
    ybot.ybot_set_vel(cos(t),sin(t),0.0)
}