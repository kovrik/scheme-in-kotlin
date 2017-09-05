package core.procedures.strings

import core.procedures.AFn

class StringLength : AFn<CharSequence?, Long>(name = "string-length", isPure = true, minArgs = 1, maxArgs = 1,
                         mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = arg!!.toString().length.toLong()
}
