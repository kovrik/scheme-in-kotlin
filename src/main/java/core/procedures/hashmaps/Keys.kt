package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.Cons

class Keys : AFn<Map<*, *>?, List<*>>(name = "keys", isPure = true, minArgs = 1, maxArgs = 1,
                                      mandatoryArgsTypes = arrayOf<Class<*>>(Map::class.java)) {

    override operator fun invoke(arg: Map<*, *>?) = Cons.list(arg!!.keys)
}
