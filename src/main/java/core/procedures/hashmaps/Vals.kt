package core.procedures.hashmaps

import core.procedures.AFn

class Vals : AFn<Map<*, *>?, Sequence<*>>(name = "vals", isPure = true, minArgs = 1, maxArgs = 1,
                                          mandatoryArgsTypes = arrayOf(Map::class.java)) {

    override operator fun invoke(arg: Map<*, *>?) = arg!!.values.asSequence()
}
