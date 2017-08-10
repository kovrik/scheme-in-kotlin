package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.MutableHashmap

class HashMapImmutableProc : AFn<Any?, Map<*, *>>(name = "hash-map-immutable", isPure = true) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size % 2 == 0 -> MutableHashmap().apply {
            for (i in 0..args.size - 1 step 2) {
                put(args[i], args[i + 1])
            }
        }.toImmutableMap()
        else -> throw IllegalArgumentException("hash-map: no value supplied for key: ${args[args.size - 1]}")
    }
}
