package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector
import core.scm.Type
import core.scm.Vector

class VectorRef : AFn<Any?, Any?>(name = "vector-ref", isPure = true, minArgs = 2, maxArgs = 2,
                      mandatoryArgsTypes = arrayOf(MutableVector::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1!! as Vector).invoke(arg2!! as Number)
}
