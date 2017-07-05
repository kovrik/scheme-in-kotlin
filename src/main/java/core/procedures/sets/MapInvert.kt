package core.procedures.sets

import core.procedures.AFn
import core.scm.Hashmap

class MapInvert : AFn<Any?, Map<*, *>>(name = "map-invert", isPure = true, minArgs = 1,
                                             mandatoryArgsTypes = arrayOf<Class<*>>(Map::class.java)) {

    override operator fun invoke(args: Array<out Any?>): Map<*, *> {
        val result = Hashmap()
        for ((key, value) in args[0]!! as Map<*, *>) {
            result.put(value, key)
        }
        return result
    }
}
