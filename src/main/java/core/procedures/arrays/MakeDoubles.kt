package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class MakeDoubles : AFn<Any?, DoubleArray>(name = "make-doubles", isPure = true, minArgs = 1, maxArgs = 2,
                                           mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                                           lastArgType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): DoubleArray {
        val length = (args[0] as Number).toInt()
        val double = if (args.size == 1) Double.MIN_VALUE else (args[1] as Number).toDouble()
        return DoubleArray(length).apply { for (i in 0..length - 1) { set(i, double) } }
    }
}
