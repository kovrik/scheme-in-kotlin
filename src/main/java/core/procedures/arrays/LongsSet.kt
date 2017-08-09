package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class LongsSet : AFn<Any?, Unit>(name = "longs-set!", minArgs = 3, maxArgs = 3,
        mandatoryArgsTypes = arrayOf(LongArray::class.java, Type.ExactNonNegativeInteger::class.java, Number::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
        (arg1!! as LongArray)[(arg2 as Number).toInt()] = (arg3!! as Number).toLong()
    }
}
