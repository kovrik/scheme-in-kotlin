package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.MutableVector
import core.scm.Vector

class VectorLength : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(Vector::class.java)).build()) {

    override val isPure = true
    override val name = "vector-length"

    override operator fun invoke(arg: Any?): Long {
        return (arg as MutableVector).size.toLong()
    }
}
