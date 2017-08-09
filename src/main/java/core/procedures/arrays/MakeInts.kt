package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class MakeInts : AFn<Any?, IntArray>(name = "make-ints", isPure = true, minArgs = 1, maxArgs = 2,
                                     mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                                     lastArgType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): IntArray {
        val length = (args[0] as Number).toInt()
        val int = if (args.size == 1) Int.MIN_VALUE else (args[1] as Number).toInt()
        return IntArray(length).apply { for (i in 0..length - 1) { set(i, int) } }
    }
}
