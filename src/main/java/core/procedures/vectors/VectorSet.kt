package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type
import core.scm.MutableVector

class VectorSet : AFn(FnArgs(min = 3, max = 3, mandatory = arrayOf(MutableVector::class.java, Type.ExactNonNegativeInteger::class.java))) {

    override val name = "vector-set!"
    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) = (arg1!! as MutableVector).set((arg2 as Number).toInt(), arg3)
}
