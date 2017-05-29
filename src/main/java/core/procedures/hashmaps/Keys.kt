package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons

class Keys : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Map::class.java))) {

    override val isPure = true
    override val name = "keys"

    override operator fun invoke(arg: Any?): Any? {
        return Cons.list((arg as Map<*, *>).keys)
    }
}
