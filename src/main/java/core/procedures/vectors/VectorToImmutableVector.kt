package core.procedures.vectors

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableVector
import core.scm.Vector

class VectorToImmutableVector : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Vector::class.java))) {

    override val name = "vector->immutable-vector"

    override operator fun invoke(arg: Any?): Vector? {
        if (arg is MutableVector) {
            return Vector(*(arg as Vector).getArray())
        }
        return arg as Vector?
    }
}