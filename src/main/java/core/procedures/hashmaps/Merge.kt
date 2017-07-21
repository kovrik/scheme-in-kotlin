package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.Hashmap

class Merge : AFn<Any?, Map<*, *>?>(name = "merge", isPure = true, restArgsType = Map::class.java) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.isEmpty() -> null
        else -> Hashmap().apply { args.forEach { putAll(it as Map<*, *>) } }
    }
}
