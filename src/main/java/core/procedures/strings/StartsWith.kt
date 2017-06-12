package core.procedures.strings

import core.procedures.AFn

class StartsWith : AFn<CharSequence?, Boolean>(name = "starts-with?", isPure = true, minArgs = 2, maxArgs = 2,
                       mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java, CharSequence::class.java)) {

    override operator fun invoke(arg1: CharSequence?, arg2: CharSequence?) = arg1!!.toString().startsWith(arg2!!.toString())
}
