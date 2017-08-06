package core.procedures.system

import core.procedures.AFn

class Sleep : AFn<Number?, Unit>(name = "sleep", minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Long::class.java)) {

    override operator fun invoke(arg: Number?) = Thread.sleep(arg!!.toLong())
}
