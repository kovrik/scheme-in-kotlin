package core.procedures.system

import core.procedures.AFn

class Sleep : AFn<Any?, Unit>(name = "sleep", minArgs = 0, maxArgs = 1,
                              lastArgType = Number::class.java) {

    override fun invoke(args: Array<out Any?>) = when (args.size) {
        0    -> Thread.sleep(0)
        else -> Thread.sleep((args[0]!! as Number).toLong())
    }
}
