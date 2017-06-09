package core.procedures.hashmaps

import core.procedures.AFn

class Merge : AFn(name = "merge", isPure = true, restArgsType = Map::class.java) {

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
