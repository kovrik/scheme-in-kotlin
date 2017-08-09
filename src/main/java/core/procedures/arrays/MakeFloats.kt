package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class MakeFloats : AFn<Any?, FloatArray>(name = "make-floats", isPure = true, minArgs = 1, maxArgs = 2,
                                         mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                                         lastArgType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): FloatArray {
        val length = (args[0] as Number).toInt()
        val float = if (args.size == 1) Float.MIN_VALUE else (args[1] as Number).toFloat()
        return FloatArray(length).apply { for (i in 0..length - 1) { set(i, float) } }
    }
}
