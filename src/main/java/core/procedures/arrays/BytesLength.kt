package core.procedures.arrays

import core.procedures.AFn

class BytesLength : AFn<ByteArray?, Long>(name = "bytes-length", isPure = true, minArgs = 1, maxArgs = 1,
                                          mandatoryArgsTypes = arrayOf<Class<*>>(ByteArray::class.java)) {

    override operator fun invoke(arg: ByteArray?) = arg!!.size.toLong()
}
