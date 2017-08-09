package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class MakeLongs : AFn<Any?, LongArray>(name = "make-longs", isPure = true, minArgs = 1, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                                       lastArgType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): LongArray {
        val length = (args[0] as Number).toInt()
        val long = if (args.size == 1) Long.MIN_VALUE else (args[1] as Number).toLong()
        return LongArray(length).apply { for (i in 0..length - 1) { set(i, long) } }
    }
}
