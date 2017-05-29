package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class Trim : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name = "trim"

    override operator fun invoke(arg: Any?): String {
        return arg!!.toString().trim()
    }
}
