package core.procedures.cons

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutablePair

class SetMcar : AFn<Any?, Any?>(name = "set-mcar!", arity = Exactly(2),
                                mandatoryArgsTypes = arrayOf(MutablePair::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) {
        (arg1 as MutablePair<Any?, Any?>).first = arg2
    }
}
