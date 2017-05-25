package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.Type

class StringRef : AFn(FnArgsBuilder().min(2).max(2)
        .mandatory(arrayOf(CharSequence::class.java, Type.ExactNonNegativeInteger::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "string-ref"

    override operator fun invoke(arg1: Any?, arg2: Any?): Any {
        val s = arg1!!.toString()
        val pos = (arg2 as Number).toLong()
        if (pos >= s.length) {
            throw IndexOutOfBoundsException(String.format("%s: value out of range: %s", name, pos))
        }
        return s[pos.toInt()]
    }
}
