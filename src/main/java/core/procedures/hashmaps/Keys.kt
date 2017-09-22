package core.procedures.hashmaps

import core.procedures.AFn

class Keys : AFn<Map<*, *>?, Sequence<*>>(name = "keys", isPure = true, minArgs = 1, maxArgs = 1,
                                          mandatoryArgsTypes = arrayOf<Class<*>>(Map::class.java)) {

    override operator fun invoke(arg: Map<*, *>?) = arg!!.keys.asSequence()
}
