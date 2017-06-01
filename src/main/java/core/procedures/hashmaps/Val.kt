package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgs

class Val : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Map.Entry::class.java))) {

    override val isPure = true
    override val name = "val"
    override operator fun invoke(arg: Any?) = (arg as Map.Entry<*, *>).value
}
