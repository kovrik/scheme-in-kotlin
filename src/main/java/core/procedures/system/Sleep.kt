package core.procedures.system

import core.exceptions.ThrowableWrapper
import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Void

class Sleep : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Long::class.java)).build()) {

    override val name: String
        get() = "sleep"

    override fun apply1(arg: Any?): Void? {
        try {
            Thread.sleep((arg as Number).toLong())
        } catch (e: InterruptedException) {
            throw ThrowableWrapper(e)
        }
        return Void.VOID
    }
}
