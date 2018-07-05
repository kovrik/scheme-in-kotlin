package core.procedures.system

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Cast : AFn<Any?, Any?>(name = "cast", isPure = true, arity = Exactly(2),
                             mandatoryArgsTypes = arrayOf(Class::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Any? = (arg1 as Class<*>).cast(arg2)
}
