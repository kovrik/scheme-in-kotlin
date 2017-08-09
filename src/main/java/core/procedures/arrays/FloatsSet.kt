package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class FloatsSet : AFn<Any?, Unit>(name = "floats-set!", minArgs = 3, maxArgs = 3,
        mandatoryArgsTypes = arrayOf(FloatArray::class.java, Type.ExactNonNegativeInteger::class.java, Number::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
        (arg1!! as FloatArray)[(arg2 as Number).toInt()] = (arg3!! as Number).toFloat()
    }
}
