package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

class StringToImmutableString : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    override val name = "string->immutable-string"

    override operator fun invoke(arg: Any?): Any? {
        when (arg) {
            is String -> return arg
            else -> return arg!!.toString()
        }
    }
}