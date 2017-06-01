package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

class EndsWith : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf<Class<*>>(CharSequence::class.java, CharSequence::class.java))) {

    override val name = "ends-with?"
    override operator fun invoke(arg1: Any?, arg2: Any?) = arg1!!.toString().endsWith(arg2!!.toString())
}
