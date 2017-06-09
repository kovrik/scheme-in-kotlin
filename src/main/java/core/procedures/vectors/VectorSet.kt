package core.procedures.vectors

import core.procedures.AFn
import core.scm.Type
import core.scm.MutableVector

class VectorSet : AFn(name = "vector-set!", minArgs = 3, maxArgs = 3,
                      mandatoryArgsTypes = arrayOf(MutableVector::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) = (arg1!! as MutableVector).set((arg2 as Number).toInt(), arg3)
}
