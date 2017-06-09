package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector
import core.scm.Vector

class VectorToImmutableVector : AFn(name = "vector->immutable-vector", isPure = true, minArgs = 1, maxArgs = 1,
                                    mandatoryArgsTypes = arrayOf<Class<*>>(Vector::class.java)) {

    override operator fun invoke(arg: Any?) = when (arg) {
        is MutableVector -> Vector(*(arg as Vector).getArray())
        else -> arg as Vector?
    }
}