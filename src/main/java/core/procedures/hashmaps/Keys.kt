package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Keys : AFn<Map<*, *>?, Sequence<*>>(name = "keys", isPure = true, arity = Exactly(1),
                                          mandatoryArgsTypes = arrayOf(Map::class.java)) {

    override operator fun invoke(arg: Map<*, *>?) = arg!!.keys.asSequence()
}
