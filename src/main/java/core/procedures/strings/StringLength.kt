package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class StringLength : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "string-length"

    override fun apply1(arg: Any?): Long {
        return arg!!.toString().length.toLong()
    }
}
