package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class ObjectsSet : AFn<Any?, Unit>(name = "objects-set!", minArgs = 3, maxArgs = 3,
        mandatoryArgsTypes = arrayOf(Array<Any?>::class.java, Type.ExactNonNegativeInteger::class.java, Any::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
        (arg1!! as Array<Any?>)[(arg2 as Number).toInt()] = arg3
    }
}
