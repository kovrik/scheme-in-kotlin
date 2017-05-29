package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

class Lowercase : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    override val name = "lower-case"

    override operator fun invoke(arg: Any?): String {
        return arg!!.toString().toLowerCase()
    }
}
