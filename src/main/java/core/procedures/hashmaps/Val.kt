package core.procedures.hashmaps

import core.procedures.AFn

class Val : AFn(name = "val", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Map.Entry::class.java)) {

    override operator fun invoke(arg: Any?) = (arg as Map.Entry<*, *>).value
}
