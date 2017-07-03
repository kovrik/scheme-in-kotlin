package core.procedures.vectors

import core.procedures.AFn
import core.scm.Vector

class VectorImmutable : AFn<Any?, Vector>(name = "vector-immutable") {

    override operator fun invoke(args: Array<out Any?>) = Vector(args)
}
