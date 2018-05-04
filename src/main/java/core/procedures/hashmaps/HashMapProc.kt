package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.MutableHashmap

class HashMapProc : AFn<Any?, Map<*, *>>(name = "hash-map", isPure = true) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size % 2 == 0 -> MutableHashmap<Any?, Any?>().apply {
            for (i in 0 until args.size step 2) {
                put(args[i], args[i + 1])
            }
        }
        else -> throw IllegalArgumentException("hash-map: no value supplied for key: ${args[args.size - 1]}")
    }
}
