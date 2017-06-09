package core.procedures.cons

import core.procedures.AFn
import core.scm.Type

class SetCar : AFn(name = "set-car!", minArgs = 2, maxArgs = 2, mandatoryArgsTypes = arrayOf(Type.Pair::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) {
        (arg1 as MutableList<Any?>)[0] = arg2
    }
}
