package core.procedures.system

import core.procedures.AFn
import core.procedures.FnArgs

class IsInstance : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(Class::class.java, Any::class.java))) {

    override val isPure = true
    override val name = "instance?"

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean? {
        return (arg1 as Class<*>).isAssignableFrom(arg2!!.javaClass)
    }
}
