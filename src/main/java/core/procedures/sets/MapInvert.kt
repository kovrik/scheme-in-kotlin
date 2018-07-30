package core.procedures.sets

import core.procedures.AFn
import core.procedures.Arity
import core.scm.MutableHashmap

class MapInvert : AFn<Map<*, *>?, Map<*, *>>(name = "map-invert", isPure = true, arity = Arity.Exactly(1),
                                             mandatoryArgsTypes = arrayOf(Map::class.java)) {

    override operator fun invoke(arg: Map<*, *>?) = arg!!.entries.associateTo(MutableHashmap()) { it.value to it.key }
}
