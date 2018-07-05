package core.procedures.vectors

import core.procedures.AFn
import core.procedures.Arity.Exactly
import core.scm.Vector

class VectorLength : AFn<Vector?, Long>(name = "vector-length", isPure = true, arity = Exactly(1),
                         mandatoryArgsTypes = arrayOf(Vector::class.java)) {

    override operator fun invoke(arg: Vector?) = arg!!.size.toLong()
}
