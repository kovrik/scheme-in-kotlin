package core.procedures.system

import core.exceptions.ThrowableWrapper
import core.procedures.AFn

class Sleep : AFn<Number?, Unit>(name = "sleep", minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Long::class.java)) {

    override operator fun invoke(arg: Number?) = try {
        Thread.sleep(arg!!.toLong())
    } catch (e: InterruptedException) {
        throw ThrowableWrapper(e)
    }
}
