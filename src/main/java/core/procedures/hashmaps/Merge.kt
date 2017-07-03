package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.InvokableMap

class Merge : AFn<Any?, Map<*, *>?>(name = "merge", isPure = true, restArgsType = Map::class.java) {

    override operator fun invoke(args: Array<out Any?>): Map<*, *>? {
        if (args.isEmpty()) {
            return null
        }
        val result = InvokableMap()
        for (m in args) {
            result.putAll(m as Map<*, *>)
        }
        return result
    }
}
