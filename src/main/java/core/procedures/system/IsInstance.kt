package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class IsInstance : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Class::class.java, Any::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "instance?"

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean? {
        return (arg1 as Class<*>).isAssignableFrom(arg2!!.javaClass)
    }
}
