package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableString

class StringToMutableString : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    override val name = "string->mutable-string"

    override operator fun invoke(arg: Any?): Any? {
        when (arg) {
            is MutableString, is StringBuilder -> return arg
            else -> return MutableString(arg!!.toString())
        }
    }
}