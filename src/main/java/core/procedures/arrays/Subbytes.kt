package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

open class Subbytes : AFn<Any?, ByteArray>(name = "subbytes", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(ByteArray::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<out Any?>): ByteArray {
        val bytes = args[0] as ByteArray
        val start = (args[1] as Number).toInt()
        if (start > bytes.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = bytes.size
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > bytes.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return bytes.copyOfRange(start, end)
    }
}
