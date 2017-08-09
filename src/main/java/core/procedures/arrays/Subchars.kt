package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

open class Subchars : AFn<Any?, CharArray>(name = "subchars", isPure = true, minArgs = 2, maxArgs = 3,
                           mandatoryArgsTypes = arrayOf(CharArray::class.java, Type.ExactNonNegativeInteger::class.java),
                           restArgsType = Type.ExactNonNegativeInteger::class.java) {

    override operator fun invoke(args: Array<out Any?>): CharArray {
        val chars = args[0] as CharArray
        val start = (args[1] as Number).toInt()
        if (start > chars.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $start")
        }
        var end = chars.size
        if (args.size == 3) {
            end = (args[2] as Number).toInt()
        }
        if (end > chars.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $end")
        }
        return chars.copyOfRange(start, end)
    }
}
