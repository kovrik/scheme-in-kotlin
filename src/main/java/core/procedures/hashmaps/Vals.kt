package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgs
import core.scm.Cons

class Vals : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Map::class.java))) {

    override val isPure = true
    override val name = "vals"
    override operator fun invoke(arg: Any?) = Cons.list((arg as Map<*, *>).values)
}
