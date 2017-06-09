package core.procedures.system

import core.exceptions.ThrowableWrapper
import core.procedures.AFn

class Sleep : AFn(name = "sleep", minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Long::class.java)) {

    override operator fun invoke(arg: Any?) = try {
        Thread.sleep((arg as Number).toLong())
    } catch (e: InterruptedException) {
        throw ThrowableWrapper(e)
    }
}
