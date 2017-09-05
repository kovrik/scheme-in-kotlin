package core.procedures.vectors

import core.procedures.AFn
import core.scm.Cons
import core.scm.Vector

class VectorToList : AFn<Vector, List<*>>(name = "vector->list", isPure = true, minArgs = 1, maxArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(Vector::class.java)) {

    override operator fun invoke(arg: Vector) = Cons.list(arg.getArray())
}