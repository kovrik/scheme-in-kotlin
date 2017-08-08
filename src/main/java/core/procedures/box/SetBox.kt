package core.procedures.box

import core.procedures.AFn
import core.scm.Box

class SetBox : AFn<Any?, Any?>(name = "set-box!", minArgs = 2, maxArgs = 2,
                               mandatoryArgsTypes = arrayOf<Class<*>>(Box::class.java, Any::class.java)) {

    override fun invoke(arg1: Any?, arg2: Any?) {
        (arg1 as Box<Any?>).set(arg2)
    }
}