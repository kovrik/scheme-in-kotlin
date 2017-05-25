package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder

import java.util.regex.Pattern

class RePattern : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name: String
        get() = "re-pattern"

    override operator fun invoke(arg: Any?): Pattern {
        return Pattern.compile(arg!!.toString())
    }
}
