package core.procedures.bytes

import core.procedures.AFn

class BytesAppend : AFn<Any?, ByteArray>(name = "bytes-append", isPure = true, restArgsType = ByteArray::class.java) {

    override operator fun invoke(args: Array<out Any?>): ByteArray {
        val bytes = mutableListOf<Byte>()
        for (arr in args) {
            for (b in (arr as ByteArray)) {
                bytes.add(b)
            }
        }
        return bytes.toByteArray()
    }
}
