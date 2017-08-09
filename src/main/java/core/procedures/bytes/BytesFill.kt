package core.procedures.bytes

import core.procedures.AFn

class BytesFill : AFn<Any?, ByteArray>(name = "bytes-fill!", isPure = true, minArgs = 2, maxArgs = 2,
                                       mandatoryArgsTypes = arrayOf<Class<*>>(ByteArray::class.java, Byte::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): ByteArray {
        val b = (arg2 as Number).toByte()
        return (arg1 as ByteArray).apply { fill(b) }
    }
}
