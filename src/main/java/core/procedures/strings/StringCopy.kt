package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgsBuilder
import core.scm.MutableString

class StringCopy : AFn(FnArgsBuilder().min(1).max(1).mandatory(arrayOf<Class<*>>(CharSequence::class.java)).build()) {

    override val name = "string-copy"

    override operator fun invoke(arg: Any?): MutableString {
        return MutableString(arg!!.toString())
    }
}
