package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableVector
import core.scm.Vector

class VectorLength : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Vector::class.java))) {

    override val isPure = true
    override val name = "vector-length"
    override operator fun invoke(arg: Any?) = (arg as MutableVector).size.toLong()
}
