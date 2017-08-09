package core.procedures.bytes

import core.procedures.AFn
import core.scm.Type

class BytesRef : AFn<Any?, Byte>(name = "bytes-ref", isPure = true, minArgs = 2, maxArgs = 2,
                      mandatoryArgsTypes = arrayOf(ByteArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Byte {
        val bytes = arg1 as ByteArray
        val pos = (arg2 as Number).toInt()
        if (pos >= bytes.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $pos")
        }
        return bytes[pos]
    }
}
