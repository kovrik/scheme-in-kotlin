package core.procedures.vectors

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Type
import core.scm.Vector

class VectorRef : AFn<Any?, Any?>(name = "vector-ref", isPure = true, arity = Exactly(2),
                      mandatoryArgsTypes = arrayOf(Vector::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1!! as Vector).invoke(arg2!! as Number)
}
