package core.procedures.cons

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type
import core.scm.Void

class SetCar : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Type.Pair::class.java, Any::class.java)).build()) {

    override val name: String
        get() = "set-car!"

    override fun apply2(arg1: Any?, arg2: Any?): Any? {
        (arg1 as MutableList<Any?>).set(0, arg2)
        return Void.VOID
    }
}
