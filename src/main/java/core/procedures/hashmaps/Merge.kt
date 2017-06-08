package core.procedures.hashmaps

import core.procedures.AFn
import core.procedures.FnArgs

class Merge : AFn(FnArgs(min = 0, rest = Map::class.java)) {

    override val isPure = true
    override val name = "merge"

    override operator fun invoke(vararg args: Any?): Any? {
        if (args.isEmpty()) {
            return Unit
        }
        val result = HashMap<Any?, Any?>()
        for (m in args) {
            result.putAll(m as Map<*, *>)
        }
        return result
    }
}
