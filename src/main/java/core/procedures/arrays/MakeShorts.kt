package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class MakeShorts : AFn<Any?, ShortArray>(name = "make-shorts", isPure = true, minArgs = 1, maxArgs = 2,
                                         mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                                         lastArgType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): ShortArray {
        val length = (args[0] as Number).toInt()
        val short = if (args.size == 1) Short.MIN_VALUE else (args[1] as Number).toShort()
        return ShortArray(length).apply { for (i in 0..length - 1) { set(i, short) } }
    }
}
