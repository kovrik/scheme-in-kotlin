package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.Cons

class Vals : AFn(name = "vals", isPure = true, minArgs = 1, maxArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Map::class.java)) {

    override operator fun invoke(arg: Any?) = Cons.list((arg as Map<*, *>).values)
}
