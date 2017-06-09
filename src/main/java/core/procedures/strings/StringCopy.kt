package core.procedures.strings

import core.procedures.AFn
import core.scm.MutableString

class StringCopy : AFn(name = "string-copy", isPure = true, minArgs = 1, maxArgs = 1,
                       mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = MutableString(arg!!.toString())
}
