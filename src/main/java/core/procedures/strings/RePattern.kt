package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

import java.util.regex.Pattern

class RePattern : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    override val name = "re-pattern"

    override operator fun invoke(arg: Any?): Pattern {
        return Pattern.compile(arg!!.toString())
    }
}
