package core.procedures.strings

import core.procedures.AFn

class Trim : AFn(name = "trim", isPure = true, minArgs = 1, maxArgs = 1,
                 mandatoryArgsTypes = arrayOf<Class<*>>(CharSequence::class.java)) {

    override operator fun invoke(arg: Any?) = arg!!.toString().trim()
}
