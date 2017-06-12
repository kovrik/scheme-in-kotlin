package core.procedures.sets

import core.procedures.AFn

class MapInvert : AFn<Map<*, *>?, Map<*, *>>(name = "map-invert", isPure = true, minArgs = 1,
                                             mandatoryArgsTypes = arrayOf<Class<*>>(Map::class.java)) {

    override operator fun invoke(vararg args: Map<*, *>?): Map<*, *> {
        val result = HashMap<Any?, Any?>()
        for ((key, value) in args[0]!!) {
            result.put(value, key)
        }
        return result
    }
}
