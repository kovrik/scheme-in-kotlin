package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs

class Cast : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(Class::class.java, Any::class.java))) {

    override val isPure = true
    override val name = "cast"
    override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1 as Class<*>).cast(arg2)
}
