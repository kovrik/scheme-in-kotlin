package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Lowercase : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name: String
        get() = "lower-case"

    override fun apply1(arg: Any?): String {
        return arg!!.toString().toLowerCase()
    }
}
