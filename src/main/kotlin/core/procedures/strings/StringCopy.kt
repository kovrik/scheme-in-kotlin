package core.procedures.strings

import core.procedures.AFn
import core.scm.MutableString

class StringCopy : AFn<CharSequence?, MutableString>(name = "string-copy", isPure = true, minArgs = 1, maxArgs = 1,
                       mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = MutableString(arg!!.toString())
}
