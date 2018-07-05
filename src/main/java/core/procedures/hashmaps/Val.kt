package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.Arity.Exactly

class Val : AFn<Map.Entry<*, *>?, Any?>(name = "val", isPure = true, arity = Exactly(1),
                                        mandatoryArgsTypes = arrayOf(Map.Entry::class.java)) {

    override operator fun invoke(arg: Map.Entry<*, *>?) = arg!!.value
}
