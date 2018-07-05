package core.procedures.box

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Box

class Reset: AFn<Any?, Any?>(name = "reset!", arity = Exactly(2),
                             mandatoryArgsTypes = arrayOf(Box::class.java, Any::class.java)) {

    override fun invoke(arg1: Any?, arg2: Any?): Any? {
        (arg1 as Box<Any?>).set(arg2)
        return arg2
    }
}