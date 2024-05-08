import bind_arduino.[serial_begin, println, delay_seconds, seconds, sin, cos, _mul_f, _add_f, _sub_f, _i2f, clip]
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
    let dt: float = $emit "(`t`-T_LAST)"
    //ybot.ybot_set_vel(cos(t),sin(t),0.0)

    $emit_at_top "static double RX = 0.0;"
    $emit_at_top "static double RV = 0.0;"

    let dist = _i2f(prizm.read_sonic_sensor_cm(2))
    let trv = _sub_f(dist, 50.0)
    trv = _add_f(
        $emit"RV",
        clip(
            _sub_f(trv, $emit"RV"),
            _mul_f(-25.0, dt),
            _mul_f(25.0, dt),
        ),
    )
    $emit_stmt "RV = (`trv`)"
    $emit_stmt "RX += RV * `dt` "

    $emit_stmt "Serial.print(RX)"
    $emit_stmt "Serial.print(\", \")"
    $emit_stmt "Serial.println(RV)"

    let tr = _mul_f(t, 1.0)
    ybot.ybot_update(
        $emit "RX",
        0.0,
        0.0,
        t,
    )
}