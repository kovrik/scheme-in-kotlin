package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.MutableString

class StringCopy : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    override val name = "string-copy"

    override operator fun invoke(arg: Any?): MutableString {
        return MutableString(arg!!.toString())
    }
}
