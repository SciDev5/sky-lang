import bind_arduino.[serial_begin, println]
import prizm

@name "setup"
fn _setup_() {
    serial_begin(9600)
    prizm.begin()
}

@name "loop"
fn _loop_() {
    println("hello world! :3c")
    prizm.set_motor_speeds(1.0, -1.0)
}