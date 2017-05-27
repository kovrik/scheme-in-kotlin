package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class StartsWith : AFn(FnArgsBuilder().min(2).max(2).mandatory(arrayOf<Class<*>>(CharSequence::class.java, CharSequence::class.java)).build()) {

    override val name = "starts-with?"

    override operator fun invoke(arg1: Any?, arg2: Any?): Boolean {
        return arg1!!.toString().startsWith(arg2!!.toString())
    }
}
