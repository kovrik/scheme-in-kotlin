package core.procedures.system

import core.procedures.AFn
import core.procedures.Arity.Range

class Sleep : AFn<Any?, Unit>(name = "sleep", arity = Range(0, 1), lastArgType = Number::class.java) {

    override fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Thread.sleep(0)
        else -> Thread.sleep((args[0]!! as Number).toLong())
    }
}
