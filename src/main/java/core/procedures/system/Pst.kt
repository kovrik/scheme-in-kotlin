package core.procedures.system

import core.procedures.AFn

class Pst : AFn(name = "pst", minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Throwable::class.java)) {

    override operator fun invoke(arg: Any?) = (arg as Throwable).printStackTrace()
}
