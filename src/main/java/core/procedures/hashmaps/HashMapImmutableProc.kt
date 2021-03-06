package core.procedures.hashmaps

import core.Writer
import core.procedures.AFn
import core.scm.MutableHashmap

class HashMapImmutableProc : AFn<Any?, Map<*, *>>(name = "hash-map-immutable", isPure = true) {

    override operator fun invoke(args: Array<out Any?>) = when {
        args.size % 2 == 0 -> MutableHashmap<Any?, Any?>().apply {
            for (i in args.indices step 2) {
                put(args[i], args[i + 1])
            }
        }.toImmutableMap()
        else -> throw IllegalArgumentException("$name: no value supplied for key: ${Writer.write(args[args.size - 1])}")
    }
}
