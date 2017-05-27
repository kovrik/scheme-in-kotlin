package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Cast : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf(Class::class.java, Any::class.java)).build()) {

    override val isPure = true
    override val name = "cast"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? {
        return (arg1 as Class<*>).cast(arg2)
    }
}
