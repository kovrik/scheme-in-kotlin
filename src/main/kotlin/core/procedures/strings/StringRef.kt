package core.procedures.strings

import core.procedures.AFn
import core.scm.Type

class StringRef : AFn<Any?, Char>(name = "string-ref", isPure = true, minArgs = 2, maxArgs = 2,
                      mandatoryArgsTypes = arrayOf(CharSequence::class.java, Type.ExactNonNegativeInteger::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?): Char {
        val str = arg1!!.toString()
        val pos = (arg2 as Number).toInt()
        if (pos >= str.length) {
            throw IndexOutOfBoundsException("$name: value out of range: $pos")
        }
        return str[pos]
    }
}
