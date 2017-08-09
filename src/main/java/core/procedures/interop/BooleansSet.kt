package core.procedures.interop

import core.procedures.AFn
import core.scm.Type

class BooleansSet : AFn<Any?, Unit>(name = "booleans-set!", minArgs = 3, maxArgs = 3,
        mandatoryArgsTypes = arrayOf(BooleanArray::class.java, Type.ExactNonNegativeInteger::class.java, Boolean::class.javaObjectType)) {

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
        (arg1!! as BooleanArray)[(arg2 as Number).toInt()] = arg3!! as Boolean
    }
}
