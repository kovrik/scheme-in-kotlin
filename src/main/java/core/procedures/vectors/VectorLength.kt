package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector
import core.scm.Vector

class VectorLength : AFn(name = "vector-length", isPure = true, minArgs = 1, maxArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(Vector::class.java)) {

    override operator fun invoke(arg: Any?) = (arg as MutableVector).size.toLong()
}
