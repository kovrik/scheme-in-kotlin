package core.procedures.strings

import core.procedures.AFn

class Includes : AFn(name = "includes?", isPure = true, minArgs = 2, maxArgs = 2,
                     mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java, CharSequence::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = arg1!!.toString().contains(arg2!!.toString())
}
