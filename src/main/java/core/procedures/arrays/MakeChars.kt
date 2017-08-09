package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class MakeChars : AFn<Any?, CharArray>(name = "make-chars", isPure = true, minArgs = 1, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                                       lastArgType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): CharArray {
        val length = (args[0] as Number).toInt()
        val char = if (args.size == 1) Character.MIN_VALUE else (args[1] as Number).toChar()
        return CharArray(length).apply { for (i in 0..length - 1) { set(i, char) } }
    }
}
