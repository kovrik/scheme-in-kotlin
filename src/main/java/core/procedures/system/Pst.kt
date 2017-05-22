package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Void

class Pst : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Throwable::class.java)).build()) {

    override val name: String
        get() = "pst"

    override fun apply1(arg: Any?): Any? {
        (arg as Throwable).printStackTrace()
        return Void.VOID
    }
}
