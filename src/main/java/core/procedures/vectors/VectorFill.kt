package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableVector

class VectorFill : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(MutableVector::class.java, Any::class.java))) {

    override val name = "vector-fill!"
    override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1!! as MutableVector).getArray().fill(arg2)
}
