package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.MutableString

class StringToMutableString : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name: String
        get() = "string->mutable-string"

    override fun apply1(arg: Any?): Any? {
        when (arg) {
            is MutableString, is StringBuilder -> return arg
            else -> return MutableString(arg!!.toString())
        }
    }
}