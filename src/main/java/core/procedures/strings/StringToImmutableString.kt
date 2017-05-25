package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder

class StringToImmutableString : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name: String
        get() = "string->immutable-string"

    override operator fun invoke(arg: Any?): Any? {
        when (arg) {
            is String -> return arg
            else -> return arg!!.toString()
        }
    }
}