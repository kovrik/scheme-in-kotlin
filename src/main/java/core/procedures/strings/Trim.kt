package core.procedures.strings

import core.procedures.AFn

class Trim : AFn<CharSequence?, String>(name = "trim", isPure = true, minArgs = 1, maxArgs = 1,
                 mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: CharSequence?) = arg!!.toString().trim()
}
