package core.procedures.strings

import core.procedures.AFn

class Includes : AFn<CharSequence?, Boolean>(name = "includes?", isPure = true, minArgs = 2, maxArgs = 2,
                     mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java, CharSequence::class.java)) {

    override operator fun invoke(arg1: CharSequence?, arg2: CharSequence?) = arg1!!.toString().contains(arg2!!.toString())
}
