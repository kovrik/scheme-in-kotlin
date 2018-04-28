package core.procedures.system

import core.procedures.AFn

class Cast : AFn<Any?, Any?>(name = "cast", isPure = true, minArgs = 2, maxArgs = 2,
                             mandatoryArgsTypes = arrayOf(Class::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? = (arg1 as Class<*>).cast(arg2)
}
