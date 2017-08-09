package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class MakeBooleans : AFn<Any?, BooleanArray>(name = "make-booleans", isPure = true, minArgs = 1, maxArgs = 2,
                                             mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                                             restArgsType = Boolean::class.java) {

    override operator fun invoke(args: Array<out Any?>): BooleanArray {
        val length = (args[0] as Number).toInt()
        val boolean = if (args.size == 1) false else args[1] as Boolean
        return BooleanArray(length).apply { for (i in 0..length - 1) { set(i, boolean) } }
    }
}
