package core.procedures.sets

import core.procedures.AFn
import core.procedures.Arity.AtLeast
import core.scm.MutableHashmap

class MapInvert : AFn<Any?, Map<*, *>>(name = "map-invert", isPure = true, arity = AtLeast(1),
                                       mandatoryArgsTypes = arrayOf(Map::class.java)) {

    override operator fun invoke(args: Array<out Any?>) = MutableHashmap<Any?, Any?>().apply {
        (args[0]!! as Map<*, *>).forEach { k, v -> put(v, k) }
    }
}
