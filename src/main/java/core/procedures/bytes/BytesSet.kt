package core.procedures.bytes

import core.procedures.AFn
import core.scm.Type

class BytesSet : AFn<Any?, Unit>(name = "bytes-set!", minArgs = 3, maxArgs = 3,
        mandatoryArgsTypes = arrayOf(ByteArray::class.java, Type.ExactNonNegativeInteger::class.java, Byte::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?, arg3: Any?) {
        (arg1!! as ByteArray)[(arg2 as Number).toInt()] = (arg3!! as Number).toByte()
    }
}
