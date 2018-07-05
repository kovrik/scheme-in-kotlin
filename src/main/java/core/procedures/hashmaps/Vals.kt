package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Vals : AFn<Map<*, *>?, Sequence<*>>(name = "vals", isPure = true, arity = Exactly(1),
                                          mandatoryArgsTypes = arrayOf(Map::class.java)) {

    override operator fun invoke(arg: Map<*, *>?) = arg!!.values.asSequence()
}
