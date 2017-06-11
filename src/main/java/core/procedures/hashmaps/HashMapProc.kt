package core.procedures.hashmaps

import core.procedures.AFn

class HashMapProc : AFn(name = "hash-map", isPure = true) {

    override operator fun invoke(vararg args: Any?): Map<Any?, Any?> {
        if (args.size % 2 != 0) {
            throw IllegalArgumentException("hash-map: no value supplied for key: ${args[args.size - 1]}")
        }
        val result = HashMap<Any?, Any?>()
        var i = 0
        while (i < args.size) {
            result.put(args[i], args[i + 1])
            i += 2
        }
        return result
    }
}
