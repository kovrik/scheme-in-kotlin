package core.procedures.arrays

import core.procedures.AFn
import core.scm.Type

class CharsRef : AFn<Any?, Char>(name = "chars-ref", isPure = true, minArgs = 2, maxArgs = 2,
                                 mandatoryArgsTypes = arrayOf(CharArray::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Char {
        val chars = arg1 as CharArray
        val pos = (arg2 as Number).toInt()
        if (pos >= chars.size) {
            throw IndexOutOfBoundsException("$name: value out of range: $pos")
        }
        return chars[pos]
    }
}
