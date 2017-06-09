package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector

class VectorFill : AFn(name = "vector-fill!", minArgs = 2, maxArgs = 2,
                       mandatoryArgsTypes = arrayOf(MutableVector::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = (arg1!! as MutableVector).getArray().fill(arg2)
}
