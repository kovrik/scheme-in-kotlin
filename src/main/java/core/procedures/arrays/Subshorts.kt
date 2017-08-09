package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

open class Subshorts : AFn<Any?, ShortArray>(name = "subshorts", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(ShortArray::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<out Any?>): ShortArray {
        val shorts = args[0] as ShortArray
        val start = (args[1] as Number).toInt()
        if (start > shorts.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = shorts.size
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > shorts.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return shorts.copyOfRange(start, end)
    }
}
