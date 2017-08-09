package core.procedures.bytes

import core.procedures.AFn

class BytesToString : AFn<ByteArray?, String?>(name = "bytes->string", isPure = true, minArgs = 1, maxArgs = 1,
                                               mandatoryArgsTypes = arrayOf<Class<*>>(ByteArray::class.java)) {

    override operator fun invoke(arg: ByteArray?) = arg?.let { String(arg) }
}
