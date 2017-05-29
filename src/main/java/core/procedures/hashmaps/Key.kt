package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgs

class Key : AFn(FnArgs(min = 1, max = 1, mandatory = arrayOf<Class<*>>(Map.Entry::class.java))) {

    override val isPure = true
    override val name = "key"

    override operator fun invoke(arg: Any?): Any? {
        return (arg as Map.Entry<*, *>).key
    }
}
