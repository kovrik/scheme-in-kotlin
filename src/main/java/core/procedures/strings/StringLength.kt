package core.procedures.strings

import core.procedures.AFn
import core.procedures.FnArgs

class StringLength : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(CharSequence::class.java))) {

    override val isPure = true
    override val name = "string-length"
    override operator fun invoke(arg: Any?) = arg!!.toString().length.toLong()
}
