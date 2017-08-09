package core.procedures.interop

import core.procedures.AFn

class Bytes : AFn<Any?, ByteArray>(name = "bytes", isPure = true, restArgsType = Number::class.java) {

    override operator fun invoke(args: Array<out Any?>): ByteArray {
        val bytes = ByteArray(args.size)
        for (i in 0..args.size - 1) {
            bytes[i] = (args[i] as Number).toByte()
        }
        return bytes
    }
}
