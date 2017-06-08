package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Type

class StringRef : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf(CharSequence::class.java, Type.ExactNonNegativeInteger::class.java))) {

    override val isPure = true
    override val name = "string-ref"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any {
        val str = arg1!!.toString()
        val pos = (arg2 as Number).toInt()
        if (pos >= str.length) {
            throw IndexOutOfBoundsException("$name: value out of range: $pos")
        }
        return str[pos]
    }
}
