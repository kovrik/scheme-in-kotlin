package core.procedures.meta

import core.procedures.AFn
import core.scm.IMeta
import core.scm.Symbol

class WIthMeta : AFn<Any?, Symbol>(name = "with-meta", minArgs = 2, maxArgs = 2,
                                   mandatoryArgsTypes = arrayOf(IMeta::class.java, Map::class.java)) {

    override operator fun invoke(arg1: Any?, arg2: Any?) = Symbol((arg1 as Symbol).name, arg2 as Map<*, *>?)
}
