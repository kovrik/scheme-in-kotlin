package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

class StartsWith : AFn(FnArgs(min = 2, max = 2, mandatory = arrayOf<Class<*>>(CharSequence::class.java, CharSequence::class.java))) {

    override val name = "starts-with?"

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        return arg1!!.toString().startsWith(arg2!!.toString())
    }
}
