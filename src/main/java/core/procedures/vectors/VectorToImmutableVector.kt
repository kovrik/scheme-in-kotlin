package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableVector
import core.scm.Vector

class VectorToImmutableVector : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Vector::class.java))) {

    override val name = "vector->immutable-vector"

    override operator fun invoke(arg: Any?) = when (arg) {
        is MutableVector -> Vector(*(arg as Vector).getArray())
        else -> arg as Vector?
    }
}