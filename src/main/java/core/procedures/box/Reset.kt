package core.procedures.box

import core.procedures.AFn
import core.scm.Box

class Reset: AFn<Any?, Any?>(name = "reset!", minArgs = 2, maxArgs = 2,
                             mandatoryArgsTypes = arrayOf(Box::class.java, Any::class.java)) {

    override fun invoke(arg1: Any?, arg2: Any?): Any? {
        (arg1 as Box<Any?>).set(arg2)
        return arg2
    }
}