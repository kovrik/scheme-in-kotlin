package core.procedures.vectors

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutableVector

class VectorFill : AFn<Any?, Unit>(name = "vector-fill!", arity = Exactly(2),
                       mandatoryArgsTypes = arrayOf(MutableVector::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1!! as MutableVector).array.fill(arg2)
}
