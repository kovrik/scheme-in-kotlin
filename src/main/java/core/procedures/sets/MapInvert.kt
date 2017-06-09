package core.procedures.sets

import core.procedures.AFn

class MapInvert : AFn(name = "map-invert", isPure = true, minArgs = 1, mandatoryArgsTypes = arrayOf<Class<*>>(Map::class.java)) {

    override operator fun invoke(vararg args: Any?): Map<Any?, Any?>? {
        val result = HashMap<Any?, Any?>()
        for ((key, value) in args[0] as Map<Any?, Any?>) {
            result.put(value, key)
        }
        return result
    }
}
