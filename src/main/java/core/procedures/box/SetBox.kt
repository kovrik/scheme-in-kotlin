package core.procedures.box

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Box

class SetBox : AFn<Any?, Any?>(name = "set-box!", arity = Exactly(2),
                               mandatoryArgsTypes = arrayOf(Box::class.java, Any::class.java)) {

    override fun invoke(arg1: Any?, arg2: Any?) {
        (arg1 as Box<Any?>).set(arg2)
    }
}