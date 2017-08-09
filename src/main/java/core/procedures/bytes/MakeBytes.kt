package core.procedures.bytes

import core.procedures.AFn
import core.scm.Type

class MakeBytes : AFn<Any?, ByteArray>(name = "make-bytes", isPure = true, minArgs = 1, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(Type.ExactNonNegativeInteger::class.java),
                                       restArgsType = Byte::class.java) {

    override operator fun invoke(args: Array<out Any?>): ByteArray {
        val length = (args[0] as Number).toInt()
        val b = if (args.size == 1) Byte.MIN_VALUE else (args[1] as Number).toByte()
        return ByteArray(length).apply { for (i in 0..length - 1) { set(i, b) } }
    }
}