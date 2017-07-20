package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.Hashmap

class Merge : AFn<Any?, Map<*, *>?>(name = "merge", isPure = true, restArgsType = Map::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.isEmpty() -> null
        else -> {
            val result = Hashmap()
            args.forEach { result.putAll(it as Map<*, *>) }
            result
        }
    }
}
