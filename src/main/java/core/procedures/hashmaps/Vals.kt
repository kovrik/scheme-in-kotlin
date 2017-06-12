package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.Cons

class Vals : AFn<Map<*, *>?, List<*>>(name = "vals", isPure = true, minArgs = 1, maxArgs = 1,
                                      mandatoryArgsTypes = arrayOf<Class<*>>(Map::class.java)) {

    override operator fun invoke(arg: Map<*, *>?) = Cons.list(arg!!.values)
}
