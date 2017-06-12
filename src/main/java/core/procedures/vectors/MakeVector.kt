package core.procedures.vectors

import core.procedures.AFn
import core.scm.MutableVector
import core.scm.Type

class MakeVector : AFn<Any?, MutableVector>(name = "make-vector", isPure = true, minArgs = 1, maxArgs = 2,
                       mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(vararg args: Any?) = MutableVector((args[0] as Number).toInt(), args.getOrNull(1))
}
