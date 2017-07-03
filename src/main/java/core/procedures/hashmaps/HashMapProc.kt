package core.procedures.hashmaps

import core.procedures.AFn
import core.scm.InvokableMap

class HashMapProc : AFn<Any?, Map<*, *>>(name = "hash-map", isPure = true) {

    override operator fun invoke(args: Array<out Any?>): Map<*, *> {
        if (args.size % 2 != 0) {
            throw IllegalArgumentException("hash-map: no value supplied for key: ${args[args.size - 1]}")
        }
        val result = InvokableMap()
        var i = 0
        while (i < args.size) {
            result.put(args[i], args[i + 1])
            i += 2
        }
        return result
    }
}
