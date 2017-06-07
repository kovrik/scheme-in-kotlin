package core.procedures.system

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgs

class Sleep : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Long::class.java))) {

    override val name = "sleep"

    override operator fun invoke(arg: Any?) = try {
        Thread.sleep((arg as Number).toLong())
    } catch (e: InterruptedException) {
        throw ThrowableWrapper(e)
    }
}
