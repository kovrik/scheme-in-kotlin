package core.procedures.hashmaps

import core.procedures.AFn

class Merge : AFn<Map<*, *>?, Map<*, *>?>(name = "merge", isPure = true, restArgsType = Map::class.java) {

    override operator fun invoke(vararg args: Map<*, *>?): Map<*, *>? {
        if (args.isEmpty()) {
            return null
        }
        val result = HashMap<Any?, Any?>()
        for (m in args) {
            result.putAll(m as Map<*, *>)
        }
        return result
    }
}
