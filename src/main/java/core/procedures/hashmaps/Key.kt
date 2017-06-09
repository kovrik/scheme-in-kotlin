package core.procedures.hashmaps

import core.procedures.AFn

class Key : AFn(name = "key", isPure = true, minArgs = 1, maxArgs = 1,
                mandatoryArgsTypes = arrayOf<Class<*>>(Map.Entry::class.java)) {

    override operator fun invoke(arg: Any?) = (arg as Map.Entry<*, *>).key
}
