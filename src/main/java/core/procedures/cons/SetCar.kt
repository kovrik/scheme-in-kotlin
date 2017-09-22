package core.procedures.cons

import core.procedures.AFn
import core.scm.MutablePair
import core.scm.Type

class SetCar : AFn<Any?, Any?>(name = "set-car!", minArgs = 2, maxArgs = 2,
                               mandatoryArgsTypes = arrayOf(Type.Pair::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) {
        when (arg1) {
            is MutablePair -> arg1.first = arg2
            else -> (arg1 as MutableList<Any?>)[0] = arg2
        }
    }
}
