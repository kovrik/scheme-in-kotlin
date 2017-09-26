package core.procedures.cons

import core.procedures.AFn
import core.scm.MutablePair

class SetMcdr : AFn<Any?, Any?>(name = "set-mcdr!", minArgs = 2, maxArgs = 2,
                                mandatoryArgsTypes = arrayOf(MutablePair::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) {
        (arg1 as MutablePair<Any?, Any?>).second = arg2
    }
}