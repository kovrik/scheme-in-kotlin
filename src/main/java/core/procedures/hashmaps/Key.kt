package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Key : AFn<Map.Entry<*, *>?, Any?>(name = "key", isPure = true, arity = Exactly(1),
                                        mandatoryArgsTypes = arrayOf(Map.Entry::class.java)) {

    override operator fun invoke(arg: Map.Entry<*, *>?) = arg!!.key
}
