package core.procedures.hashmaps

import core.procedures.AFn

import java.util.HashMap

class HashMapProc : AFn() {

    override val isPure: Boolean
        get() = true

    override val name: String
        get() = "hash-map"

    override fun apply(vararg args: Any?): Map<Any?, Any?> {
        if (args.size % 2 != 0) {
            throw IllegalArgumentException("hash-map: no value supplied for key: " + args[args.size - 1])
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
