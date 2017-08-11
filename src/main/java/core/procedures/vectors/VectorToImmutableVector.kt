package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector
import core.scm.Vector

class VectorToImmutableVector : AFn<Vector?, Vector>(name = "vector->immutable-vector", isPure = true, minArgs = 1, maxArgs = 1,
                                    mandatoryArgsTypes = arrayOf<Class<*>>(Vector::class.java)) {

    override operator fun invoke(arg: Vector?) = when (arg) {
        is MutableVector -> Vector(arg)
        else -> arg as Vector
    }
}