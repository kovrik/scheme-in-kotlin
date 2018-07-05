package core.procedures.vectors

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.MutableVector
import core.scm.Vector

class VectorToImmutableVector : AFn<Vector?, Vector>(name = "vector->immutable-vector", isPure = true, arity = Exactly(1),
                                                     mandatoryArgsTypes = arrayOf(Vector::class.java)) {

    override operator fun invoke(arg: Vector?) = when (arg) {
        is MutableVector -> Vector(arg)
        else -> arg as Vector
    }
}