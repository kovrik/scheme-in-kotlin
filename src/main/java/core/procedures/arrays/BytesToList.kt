package core.procedures.arrays

import core.procedures.AFn

class BytesToList : AFn<ByteArray?, List<Byte>?>(name = "bytes->list", isPure = true, minArgs = 1, maxArgs = 1,
                                                 mandatoryArgsTypes = arrayOf<Class<*>>(ByteArray::class.java)) {

    override operator fun invoke(arg: ByteArray?) = arg?.toList()
}
